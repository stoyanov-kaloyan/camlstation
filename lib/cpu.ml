type registers = {
  mutable gp : int array;
  mutable hi : int;
  mutable lo : int;
  mutable pc : int;
  mutable delayed_branch : int option;
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
type two = int * int
type one = int

type instruction =
  | ADD of three
  | SUB of three
  | ADDI of three
  | ADDIU of three
  | ADDU of three
  | SUBU of three
  | AND of three
  | ANDI of three
  | OR of three
  | ORI of three
  | NOR of three
  | XOR of three
  | XORI of three
  | BREAK
  | SYSCALL
  | MULT of two
  | MULTU of two
  | DIV of two
  | DIVU of two
  | MFC0 of two
  | MFHI of one
  | MFLO of one
  | MTC0 of two
  | MTHI of one
  | MTLO of one
  | J of one
  | JAL of one
  | JALR of two
  | JR of one
  | RFE
  | BQEZ of two
  | BEQ of three
  | BNE of three

type cpu = { mutable pc : int; mutable regs : registers; mutable cp0 : cp0 }

let mask16 imm = imm land 0xFFFF

let to32 v =
  let masked = v land 0xFFFFFFFF in
  if masked land 0x80000000 <> 0 then masked lor lnot 0xFFFFFFFF else masked

let ext16 v = if v land 0x8000 <> 0 then v lor lnot 0xFFFF else v land 0xFFFF

let ext32 v =
  let masked = v land 0xFFFFFFFF in
  if masked land 0x80000000 <> 0 then masked lor lnot 0xFFFFFFFF else masked

let to_u32 v = Int64.logand (Int64.of_int v) 0xFFFFFFFFL
let to_s32 v = Int64.of_int32 (Int32.of_int v)
let ovf_add a b res = a lxor res land (b lxor res) land 0x80000000 <> 0
let ovf_sub a b res = a lxor b land (a lxor res) land 0x80000000 <> 0
let no_ovf _ _ _ = false

let assert_valid_register (reg_num : int) : unit =
  if not (reg_num >= 0 && reg_num < 32) then
    raise
      (Invalid_argument ("Invalid register number: " ^ string_of_int reg_num))

let multu_op a b =
  let a = a land 0xFFFFFFFF in
  let b = b land 0xFFFFFFFF in
  let ah = a lsr 16 and al = a land 0xFFFF in
  let bh = b lsr 16 and bl = b land 0xFFFF in

  let p0 = al * bl in
  let p1 = al * bh in
  let p2 = ah * bl in
  let p3 = ah * bh in

  let mid = p1 + p2 + (p0 lsr 16) in
  let lo = p0 land 0xFFFF lor ((mid land 0xFFFF) lsl 16) in
  let hi = p3 + (mid lsr 16) in
  (hi, lo)

let mult_op a b =
  let a_neg = a land 0x80000000 <> 0 in
  let b_neg = b land 0x80000000 <> 0 in
  let a_abs =
    if a_neg then (lnot a + 1) land 0xFFFFFFFF else a land 0xFFFFFFFF
  in
  let b_abs =
    if b_neg then (lnot b + 1) land 0xFFFFFFFF else b land 0xFFFFFFFF
  in

  let hi, lo = multu_op a_abs b_abs in

  if a_neg <> b_neg then
    let lo_neg = (lnot lo + 1) land 0xFFFFFFFF in
    let carry = if lo_neg = 0 then 1 else 0 in
    let hi_neg = (lnot hi + carry) land 0xFFFFFFFF in
    (hi_neg, lo_neg)
  else (hi, lo)

let generic_div is_signed a b =
  let a_val = if is_signed then ext32 a else a land 0xFFFFFFFF in
  let b_val = if is_signed then ext32 b else b land 0xFFFFFFFF in
  if b_val = 0 then (0, 0)
  else if is_signed && a_val = -0x80000000 && b_val = -1 then (0, -0x80000000)
  else (a_val mod b_val, a_val / b_val)

let div_op = generic_div true
let divu_op = generic_div false

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

  let c0_of_reg (reg_num : int) : int =
    match reg_num with
    | 12 -> cpu.cp0.sr
    | 13 -> cpu.cp0.cause
    | 14 -> cpu.cp0.epc
    | 8 -> cpu.cp0.badvaddr
    | 4 -> cpu.cp0.context
    | _ ->
        raise
          (Invalid_argument
             ("Invalid coprocessor register number: " ^ string_of_int reg_num))
  in

  let set_c0_reg (reg_num : int) (value : int) : unit =
    match reg_num with
    | 12 -> cpu.cp0.sr <- value
    | 13 -> cpu.cp0.cause <- value
    | 14 -> cpu.cp0.epc <- value
    | 8 -> cpu.cp0.badvaddr <- value
    | 4 -> cpu.cp0.context <- value
    | _ ->
        raise
          (Invalid_argument
             ("Invalid coprocessor register number: " ^ string_of_int reg_num))
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

  let exec_hilo op rs rt =
    let a = get_reg rs in
    let b = get_reg rt in
    let hi, lo = op a b in
    cpu.regs.hi <- ext32 hi;
    cpu.regs.lo <- ext32 lo
  in

  let next_pc, in_delay_slot =
    match cpu.regs.delayed_branch with
    | Some target ->
        cpu.regs.delayed_branch <- None;
        (target, true)
    | None -> (cpu.pc + 4, false)
  in
  (try
     match instr with
     | ADD (rd, rs, rt) -> exec_rtype ( + ) ovf_add rd rs rt
     | SUB (rd, rs, rt) -> exec_rtype ( - ) ovf_sub rd rs rt
     | ADDU (rd, rs, rt) -> exec_rtype ( + ) no_ovf rd rs rt
     | SUBU (rd, rs, rt) -> exec_rtype ( - ) no_ovf rd rs rt
     | MULT (rs, rt) -> exec_hilo mult_op rs rt
     | MULTU (rs, rt) -> exec_hilo multu_op rs rt
     | DIV (rs, rt) -> exec_hilo div_op rs rt
     | DIVU (rs, rt) -> exec_hilo divu_op rs rt
     | AND (rd, rs, rt) -> exec_rtype ( land ) no_ovf rd rs rt
     | ADDI (rt, rs, imm) -> exec_itype ( + ) ovf_add rt rs (ext16 imm)
     | ADDIU (rt, rs, imm) -> exec_itype ( + ) no_ovf rt rs (ext16 imm)
     | ANDI (rt, rs, imm) -> exec_itype ( land ) no_ovf rt rs (mask16 imm)
     | OR (rd, rs, rt) -> exec_rtype ( lor ) no_ovf rd rs rt
     | ORI (rt, rs, imm) -> exec_itype ( lor ) no_ovf rt rs (mask16 imm)
     | NOR (rd, rs, rt) ->
         exec_rtype (fun a b -> lnot (a lor b)) no_ovf rd rs rt
     | XOR (rd, rs, rt) -> exec_rtype ( lxor ) no_ovf rd rs rt
     | XORI (rt, rs, imm) -> exec_itype ( lxor ) no_ovf rt rs (mask16 imm)
     | BREAK -> raise_exception Break
     | SYSCALL -> raise_exception Syscall
     | MFC0 (rt, rd) -> set_reg rt (c0_of_reg rd)
     | MTC0 (rt, rd) -> set_c0_reg rd (get_reg rt)
     | MFHI rd -> set_reg rd cpu.regs.hi
     | MFLO rd -> set_reg rd cpu.regs.lo
     | MTHI rs -> cpu.regs.hi <- get_reg rs
     | MTLO rs -> cpu.regs.lo <- get_reg rs
     | J target ->
         let target_addr = cpu.pc land 0xF0000000 lor (target lsl 2) in
         cpu.regs.delayed_branch <- Some target_addr
     | JAL target ->
         set_reg 31 (cpu.pc + 8);
         let target_addr = cpu.pc land 0xF0000000 lor (target lsl 2) in
         cpu.regs.delayed_branch <- Some target_addr
     | JALR (rd, rs) ->
         set_reg rd (cpu.pc + 8);
         cpu.regs.delayed_branch <- Some (get_reg rs)
     | JR rs -> cpu.regs.delayed_branch <- Some (get_reg rs)
     | RFE ->
         let mode_bits = cpu.cp0.sr land 0x3F in
         let sr_cleared = cpu.cp0.sr land lnot 0x3F in
         cpu.cp0.sr <- sr_cleared lor ((mode_bits lsr 2) land 0x3F)
     | BQEZ (rs, offset) ->
         if get_reg rs = 0 then
           let target_addr = cpu.pc + 4 + (ext16 offset lsl 2) in
           cpu.regs.delayed_branch <- Some target_addr
     | BEQ (rs, rt, offset) ->
         if get_reg rs = get_reg rt then
           let target_addr = cpu.pc + 4 + (ext16 offset lsl 2) in
           cpu.regs.delayed_branch <- Some target_addr
     | BNE (rs, rt, offset) ->
         if get_reg rs <> get_reg rt then
           let target_addr = cpu.pc + 4 + (ext16 offset lsl 2) in
           cpu.regs.delayed_branch <- Some target_addr
   with Invalid_argument msg -> print_endline ("Error: " ^ msg));

  cpu.pc <- next_pc
