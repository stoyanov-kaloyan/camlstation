type registers = {
  mutable gp : int array;
  mutable hi : int;
  mutable lo : int;
  mutable pc : int;
  delayed_branch : int option;
}

type cp0 = {
  mutable sr : int;
  mutable cause : int;
  mutable epc : int;
  mutable badvaddr : int;
  mutable context : int;
}

type cpu_exception =
  | Interrupt
  | ProtectionFault of int
  | TlbErrorLoad of int
  | TlbErrorStore of int
  | AddressErrorLoad of int
  | AddressErrorStore of int
  | BusErrorFetch
  | BusErrorLoadOrStore
  | Syscall
  | Break
  | ReservedInstruction
  | CoprocessorUnusable
  | ArithmeticOverflow

let code_of_exception (exc : cpu_exception) : int =
  match exc with
  | Interrupt -> 0
  | ProtectionFault _ -> 1
  | TlbErrorLoad _ -> 2
  | TlbErrorStore _ -> 3
  | AddressErrorLoad _ -> 4
  | AddressErrorStore _ -> 5
  | BusErrorFetch -> 6
  | BusErrorLoadOrStore -> 7
  | Syscall -> 8
  | Break -> 9
  | ReservedInstruction -> 10
  | CoprocessorUnusable -> 11
  | ArithmeticOverflow -> 12

type three = int * int * int

type instruction =
  | ADD of three
  | SUB of three
  | ADDI of three
  | ADDIU of three
  | ADDU of three
  | SUBU of three
  | AND of three
  | ANDI of three

type cpu = { mutable pc : int; mutable regs : registers; mutable cp0 : cp0 }

let to32 v =
  let masked = v land 0xFFFFFFFF in
  if masked land 0x80000000 <> 0 then masked lor lnot 0xFFFFFFFF else masked

let ext16 imm =
  if imm land 0x8000 <> 0 then imm lor lnot 0xFFFF else imm land 0xFFFF

let ovf_add a b res = a lxor res land (b lxor res) land 0x80000000 <> 0
let ovf_sub a b res = a lxor b land (a lxor res) land 0x80000000 <> 0
let no_ovf _ _ _ = false

let assert_valid_register (reg_num : int) : unit =
  if not (reg_num >= 0 && reg_num < 32) then
    raise
      (Invalid_argument ("Invalid register number: " ^ string_of_int reg_num))

(* execute mutates the state *)
let execute (cpu : cpu) (instr : instruction) : unit =
  let get_reg (reg_num : int) : int =
    if reg_num < 0 || reg_num >= Array.length cpu.regs.gp then
      raise (Invalid_argument "Register number out of bounds")
    else cpu.regs.gp.(reg_num)
  in

  let set_reg (reg_num : int) (value : int) : unit =
    assert_valid_register reg_num;
    if reg_num = 0 then () (* noop *) else cpu.regs.gp.(reg_num) <- value
  in

  let raise_exception (exc : cpu_exception) : unit =
    let code = code_of_exception exc in
    cpu.cp0.epc <- cpu.pc;

    (* TODO verify this cause clanker said it *)
    let cause_exccode_mask = lnot 0x7C in
    cpu.cp0.cause <- cpu.cp0.cause land cause_exccode_mask lor (code lsl 2);

    let code = code_of_exception exc in
    print_endline ("Raising exception with code: " ^ string_of_int code);
    (match exc with
    | ProtectionFault addr | AddressErrorLoad addr | AddressErrorStore addr ->
        cpu.cp0.badvaddr <- addr
    | TlbErrorLoad addr | TlbErrorStore addr ->
        cpu.cp0.badvaddr <- addr;
        (* Update Context Register (BadVPN2 is bits 20:4) *)
        let vpn = (addr lsr 12) land 0x7FFFF in
        let context_mask = lnot (0x7FFFF lsl 4) in
        cpu.cp0.context <- cpu.cp0.context land context_mask lor (vpn lsl 4)
    | _ -> ());

    let mode_bits = cpu.cp0.sr land 0x3F in
    let sr_cleared = cpu.cp0.sr land lnot 0x3F in
    cpu.cp0.sr <- sr_cleared lor ((mode_bits lsl 2) land 0x3F);

    let bev_set = cpu.cp0.sr land (1 lsl 22) <> 0 in

    let is_utlb_miss =
      match exc with TlbErrorLoad _ | TlbErrorStore _ -> true | _ -> false
    in

    cpu.pc <-
      (match (bev_set, is_utlb_miss) with
      | true, true -> 0xBFC00100
      | true, false -> 0xBFC00180
      | false, true -> 0x80000000
      | false, false -> 0x80000080)
  in

  let exec_rtype op check_ovf rd rs rt =
    let a = get_reg rs in
    let b = get_reg rt in
    let res = op a b in
    if check_ovf a b res then raise_exception ArithmeticOverflow
    else set_reg rd (to32 res)
  in

  let exec_itype op check_ovf rt rs imm_val =
    let a = get_reg rs in
    let res = op a imm_val in
    if check_ovf a imm_val res then raise_exception ArithmeticOverflow
    else set_reg rt (to32 res)
  in

  try
    match instr with
    | ADD (rd, rs, rt) -> exec_rtype ( + ) ovf_add rd rs rt
    | SUB (rd, rs, rt) -> exec_rtype ( - ) ovf_sub rd rs rt
    | ADDU (rd, rs, rt) -> exec_rtype ( + ) no_ovf rd rs rt
    | SUBU (rd, rs, rt) -> exec_rtype ( - ) no_ovf rd rs rt
    | AND (rd, rs, rt) -> exec_rtype ( land ) no_ovf rd rs rt
    | ADDI (rt, rs, imm) -> exec_itype ( + ) ovf_add rt rs (ext16 imm)
    | ADDIU (rt, rs, imm) -> exec_itype ( + ) no_ovf rt rs (ext16 imm)
    | ANDI (rt, rs, imm) -> exec_itype ( land ) no_ovf rt rs (imm land 0xFFFF)
  with Invalid_argument msg -> print_endline ("Error: " ^ msg)
