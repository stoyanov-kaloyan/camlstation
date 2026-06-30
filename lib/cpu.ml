type registers = {
  mutable gp : int array;
  mutable hi : int;
  mutable lo : int;
  mutable delayed_branch : int option;
}

type cp0 = {
  mutable index : int;
  mutable random : int;
  mutable entrylo0 : int;
  mutable entrylo1 : int;
  mutable context : int;
  mutable pagemask : int;
  mutable wired : int;
  mutable badvaddr : int;
  mutable count : int;
  mutable entryhi : int;
  mutable compare : int;
  mutable sr : int;
  mutable cause : int;
  mutable epc : int;
  mutable prid : int;
  mutable config : int;
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
  | SLT of three
  | SLTU of three
  | SLTI of three
  | SLTIU of three
  | AND of three
  | ANDI of three
  | OR of three
  | ORI of three
  | NOR of three
  | XOR of three
  | XORI of three
  | SLL of three
  | LUI of two
  | LB of three
  | LBU of three
  | LH of three
  | LHU of three
  | LW of three
  | SB of three
  | SH of three
  | SW of three
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

type cpu = {
  mutable bus : int array;
  mutable pc : int;
  mutable regs : registers;
  mutable cp0 : cp0;
}

let cpu_of_bios bios =
  let bus = bios in
  {
    bus;
    pc = 0x0000000;
    regs = { gp = Array.make 32 0; hi = 0; lo = 0; delayed_branch = None };
    cp0 =
      {
        index = 0;
        random = 0;
        entrylo0 = 0;
        entrylo1 = 0;
        context = 0;
        pagemask = 0;
        wired = 0;
        badvaddr = 0;
        count = 0;
        entryhi = 0;
        compare = 0;
        sr = 0;
        cause = 0;
        epc = 0;
        prid = 0;
        config = 0;
      };
  }

let bus_size = 4 * 1024 * 1024
let bus_addr addr = addr land (bus_size - 1)

let read_word (bus : int array) (addr : int) : int =
  let a0 = bus_addr addr in
  let a1 = bus_addr (addr + 1) in
  let a2 = bus_addr (addr + 2) in
  let a3 = bus_addr (addr + 3) in
  ((bus.(a3) land 0xFF) lsl 24)
  lor ((bus.(a2) land 0xFF) lsl 16)
  lor ((bus.(a1) land 0xFF) lsl 8)
  lor (bus.(a0) land 0xFF)

let write_word (bus : int array) (addr : int) (value : int) : unit =
  let a0 = bus_addr addr in
  let a1 = bus_addr (addr + 1) in
  let a2 = bus_addr (addr + 2) in
  let a3 = bus_addr (addr + 3) in
  bus.(a0) <- value land 0xFF;
  bus.(a1) <- (value lsr 8) land 0xFF;
  bus.(a2) <- (value lsr 16) land 0xFF;
  bus.(a3) <- (value lsr 24) land 0xFF

let read_byte (bus : int array) (addr : int) : int =
  let b = bus.(bus_addr addr) land 0xFF in
  if b land 0x80 <> 0 then b lor lnot 0xFF else b

let read_byte_u (bus : int array) (addr : int) : int =
  bus.(bus_addr addr) land 0xFF

let read_halfword (bus : int array) (addr : int) : int =
  let a0 = bus_addr addr in
  let a1 = bus_addr (addr + 1) in
  let h = ((bus.(a1) land 0xFF) lsl 8) lor (bus.(a0) land 0xFF) in
  if h land 0x8000 <> 0 then h lor lnot 0xFFFF else h

let read_halfword_u (bus : int array) (addr : int) : int =
  let a0 = bus_addr addr in
  let a1 = bus_addr (addr + 1) in
  ((bus.(a1) land 0xFF) lsl 8) lor (bus.(a0) land 0xFF)

let write_byte (bus : int array) (addr : int) (value : int) : unit =
  bus.(bus_addr addr) <- value land 0xFF

let write_halfword (bus : int array) (addr : int) (value : int) : unit =
  let a0 = bus_addr addr in
  let a1 = bus_addr (addr + 1) in
  bus.(a0) <- value land 0xFF;
  bus.(a1) <- (value lsr 8) land 0xFF

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

let check_for_tty_output (cpu : cpu) =
  if
    (cpu.pc = 0xA0 && cpu.regs.gp.(9) = 0x3C)
    || (cpu.pc = 0xB0 && cpu.regs.gp.(9) = 0x3D)
  then
    let char_code = cpu.regs.gp.(4) land 0xFF in
    print_char (Char.chr char_code)

exception CpuException of cpu_exception

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
    | 0 -> cpu.cp0.index
    | 1 -> cpu.cp0.random
    | 2 -> cpu.cp0.entrylo0
    | 3 -> cpu.cp0.entrylo1
    | 4 -> cpu.cp0.context
    | 5 -> cpu.cp0.pagemask
    | 6 -> cpu.cp0.wired
    | 8 -> cpu.cp0.badvaddr
    | 9 -> cpu.cp0.count
    | 10 -> cpu.cp0.entryhi
    | 11 -> cpu.cp0.compare
    | 12 -> cpu.cp0.sr
    | 13 -> cpu.cp0.cause
    | 14 -> cpu.cp0.epc
    | 15 -> cpu.cp0.prid
    | 16 -> cpu.cp0.config
    | _ ->
        raise
          (Invalid_argument
             ("Invalid coprocessor register number: " ^ string_of_int reg_num))
  in

  let set_c0_reg (reg_num : int) (value : int) : unit =
    match reg_num with
    | 0 -> cpu.cp0.index <- value
    | 1 -> cpu.cp0.random <- value
    | 2 -> cpu.cp0.entrylo0 <- value
    | 3 -> cpu.cp0.entrylo1 <- value
    | 4 -> cpu.cp0.context <- value
    | 5 -> cpu.cp0.pagemask <- value
    | 6 -> cpu.cp0.wired <- value
    | 8 -> cpu.cp0.badvaddr <- value
    | 9 -> cpu.cp0.count <- value
    | 10 -> cpu.cp0.entryhi <- value
    | 11 -> cpu.cp0.compare <- value
    | 12 -> cpu.cp0.sr <- value
    | 13 -> cpu.cp0.cause <- value
    | 14 -> cpu.cp0.epc <- value
    | 15 -> cpu.cp0.prid <- value
    | 16 -> cpu.cp0.config <- value
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
      | false, false -> 0x80000080);
    raise (CpuException exc)
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
  ignore in_delay_slot;
  (try
     (match instr with
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
      | SLT (rd, rs, rt) ->
          let res = if ext32 (get_reg rs) < ext32 (get_reg rt) then 1 else 0 in
          set_reg rd res
      | SLTU (rd, rs, rt) ->
          let res =
            if (get_reg rs land 0xFFFFFFFF) < (get_reg rt land 0xFFFFFFFF) then
              1
            else 0
          in
          set_reg rd res
      | SLTI (rt, rs, imm) ->
          let res = if ext32 (get_reg rs) < ext16 imm then 1 else 0 in
          set_reg rt res
      | SLTIU (rt, rs, imm) ->
          let res =
            if (get_reg rs land 0xFFFFFFFF) < (ext16 imm land 0xFFFFFFFF) then
              1
            else 0
          in
          set_reg rt res
      | SLL (rd, rt, shamt) -> set_reg rd ((get_reg rt lsl shamt) land 0xFFFFFFFF)
      | LUI (rt, imm) -> set_reg rt ((imm land 0xFFFF) lsl 16)
      | LB (rt, rs, imm) ->
          set_reg rt (read_byte cpu.bus (get_reg rs + ext16 imm))
      | LBU (rt, rs, imm) ->
          set_reg rt (read_byte_u cpu.bus (get_reg rs + ext16 imm))
      | LH (rt, rs, imm) ->
          set_reg rt (read_halfword cpu.bus (get_reg rs + ext16 imm))
      | LHU (rt, rs, imm) ->
          set_reg rt (read_halfword_u cpu.bus (get_reg rs + ext16 imm))
      | LW (rt, rs, imm) ->
          set_reg rt (read_word cpu.bus (get_reg rs + ext16 imm))
      | SB (rt, rs, imm) ->
          write_byte cpu.bus (get_reg rs + ext16 imm) (get_reg rt)
      | SH (rt, rs, imm) ->
          write_halfword cpu.bus (get_reg rs + ext16 imm) (get_reg rt)
      | SW (rt, rs, imm) ->
          write_word cpu.bus (get_reg rs + ext16 imm) (get_reg rt)
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
            cpu.regs.delayed_branch <- Some target_addr);
     cpu.pc <- next_pc
   with
   | CpuException _ -> ()
   (* Any other unexpected error (Invalid_argument, etc.) is fatal and
      propagates so we do not silently skip broken instructions. *))


exception UnknownOpcode of int
exception UnknownFunction of int
(* register exception printer *)
let string_of_bin ~width n =
  let rec aux n acc =
    if n = 0 then acc
    else aux (n lsr 1) ((if n land 1 = 0 then "0" else "1") ^ acc)
  in
  let raw = if n = 0 then "0" else aux n "" in
  let len = String.length raw in
  if len >= width then raw
  else String.make (width - len) '0' ^ raw

let () =
  Printexc.register_printer (function
    | UnknownOpcode code ->
        Some (Printf.sprintf "Unknown Opcode: 0x%08X" code)
    | UnknownFunction funct ->
        Some (Printf.sprintf "Unknown Function: %s" (string_of_bin ~width:6 funct))
    | _ -> None 
  )

(** given a word - 32bit - decode the opcode*)
let parse_opcode (instr : int) : instruction =
  let opcode = (instr lsr 26) land 0x3F in
  let rs = (instr lsr 21) land 0x1F in
  let rt = (instr lsr 16) land 0x1F in
  let rd = (instr lsr 11) land 0x1F in
  let shamt = (instr lsr 6) land 0x1F in
  let funct = instr land 0x3F in
  let imm = instr land 0xFFFF in
  let target = instr land 0x3FFFFFF in
  ignore shamt;
  match opcode with
  | 0b000000 -> (
      match funct with
      | 0b100000 -> ADD (rd, rs, rt)
      | 0b100001 -> ADDU (rd, rs, rt)
      | 0b100010 -> SUB (rd, rs, rt)
      | 0b100011 -> SUBU (rd, rs, rt)
      | 0b100100 -> AND (rd, rs, rt)
      | 0b100101 -> OR (rd, rs, rt)
      | 0b100110 -> XOR (rd, rs, rt)
      | 0b100111 -> NOR (rd, rs, rt)
      | 0b101010 -> SLT (rd, rs, rt)
      | 0b101011 -> SLTU (rd, rs, rt)
      | 0b011000 -> MULT (rs, rt)
      | 0b011001 -> MULTU (rs, rt)
      | 0b011010 -> DIV (rs, rt)
      | 0b011011 -> DIVU (rs, rt)
      | 0b010000 -> MFHI rd
      | 0b010010 -> MFLO rd
      | 0b010001 -> MTHI rs
      | 0b010011 -> MTLO rs
      | 0b000000 -> SLL (rd, rt, shamt)
      | 0b001000 -> JR rs
      | 0b001001 -> JALR (rd, rs)
      | 0b001100 -> SYSCALL
      | 0b001101 -> BREAK
      | _ -> raise (UnknownFunction funct))
  | 0b001000 -> ADDI (rt, rs, imm)
  | 0b001001 -> ADDIU (rt, rs, imm)
  | 0b001010 -> SLTI (rt, rs, imm)
  | 0b001011 -> SLTIU (rt, rs, imm)
  | 0b001100 -> ANDI (rt, rs, imm)
  | 0b001101 -> ORI (rt, rs, imm)
  | 0b001110 -> XORI (rt, rs, imm)
  | 0b001111 -> LUI (rt, imm)
  | 0b100000 -> LB (rt, rs, imm)
  | 0b100100 -> LBU (rt, rs, imm)
  | 0b100001 -> LH (rt, rs, imm)
  | 0b100101 -> LHU (rt, rs, imm)
  | 0b100011 -> LW (rt, rs, imm)
  | 0b101000 -> SB (rt, rs, imm)
  | 0b101001 -> SH (rt, rs, imm)
  | 0b101011 -> SW (rt, rs, imm)
  | 0b000010 -> J target
  | 0b000011 -> JAL target
  | 0b000100 -> BEQ (rs, rt, imm)
  | 0b000101 -> BNE (rs, rt, imm)
  | 0b000001 -> BQEZ (rs, imm)
  | 0b010000 -> (
      match rs with
      | 0b00000 -> MFC0 (rt, rd)
      | 0b00100 -> MTC0 (rt, rd)
      | 0b10000 -> RFE
      | _ -> raise (UnknownOpcode instr))
  | _ -> raise (UnknownOpcode instr)

let step (cpu : cpu) : unit =
  let opcode = read_word cpu.bus cpu.pc in
  let instr = parse_opcode opcode in
  execute cpu instr;
  check_for_tty_output cpu