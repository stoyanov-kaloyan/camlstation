type registers = {
  mutable gp : int array;
  mutable hi : int;
  mutable lo : int;
  mutable delayed_branch : (int * int) option;
}

type cp0 = {
  mutable index : int;
  mutable random : int;
  mutable entrylo0 : int;
  mutable entrylo1 : int;
  mutable context : int;
  mutable pagemask : int;
  mutable wired : int;
  mutable reserved7 : int;
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
  | SLL of three
  | SRL of three
  | SRA of three
  | SLLV of three
  | SRLV of three
  | SRAV of three
  | SLT of three
  | SLTU of three
  | SLTI of three
  | SLTIU of three
  | MOVZ of three
  | MOVN of three
  | LUI of two
  | LB of three
  | LBU of three
  | LH of three
  | LHU of three
  | LW of three
  | LWL of three
  | LWR of three
  | SB of three
  | SH of three
  | SW of three
  | SWL of three
  | SWR of three
  | BREAK
  | SYSCALL
  | MULT of two
  | MULTU of two
  | DIV of two
  | DIVU of two
  | MFC0 of two
  | MFHI of int
  | MFLO of int
  | MTC0 of two
  | MTHI of int
  | MTLO of int
  | J of int
  | JAL of int
  | JALR of two
  | JR of int
  | RFE
  | BLTZ of two
  | BGEZ of two
  | BLTZAL of two
  | BGEZAL of two
  | BEQ of three
  | BNE of three
  | BGTZ of two
  | BLEZ of two

type cpu = {
  mutable ram : int array;
  mutable bios : int array;
  mutable scratchpad : int array;
  mutable cache : int array;
  mutable pc : int;
  mutable regs : registers;
  mutable cp0 : cp0;
  mutable i_stat : int;
  mutable i_mask : int;
  mutable cycle_count : int;
}

let cpu_of_bios bios =
  {
    ram = Array.make (2 * 1024 * 1024) 0;
    bios;
    scratchpad = Array.make 1024 0;
    cache = Array.make (4 * 1024) 0;
    pc = 0xBFC00000;
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
        reserved7 = 0;
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
    i_stat = 0;
    i_mask = 0;
    cycle_count = 0;
  }

(* Translate a virtual address to a physical address using the PS1's
   fixed memory map. KSEG0/KSEG1/KSEG2 are mapped by clearing the top 3 bits. *)
let phys_addr addr =
  if addr land 0x80000000 <> 0 then addr land 0x1FFFFFFF else addr

let read_word_array (arr : int array) (addr : int) : int =
  ((arr.(addr + 3) land 0xFF) lsl 24)
  lor ((arr.(addr + 2) land 0xFF) lsl 16)
  lor ((arr.(addr + 1) land 0xFF) lsl 8)
  lor (arr.(addr) land 0xFF)

let write_word_array (arr : int array) (addr : int) (value : int) : unit =
  arr.(addr) <- value land 0xFF;
  arr.(addr + 1) <- (value lsr 8) land 0xFF;
  arr.(addr + 2) <- (value lsr 16) land 0xFF;
  arr.(addr + 3) <- (value lsr 24) land 0xFF

let read_byte_array (arr : int array) (addr : int) : int =
  let b = arr.(addr) land 0xFF in
  if b land 0x80 <> 0 then b lor lnot 0xFF else b

let read_byte_u_array (arr : int array) (addr : int) : int =
  arr.(addr) land 0xFF

let read_halfword_array (arr : int array) (addr : int) : int =
  let h = ((arr.(addr + 1) land 0xFF) lsl 8) lor (arr.(addr) land 0xFF) in
  if h land 0x8000 <> 0 then h lor lnot 0xFFFF else h

let read_halfword_u_array (arr : int array) (addr : int) : int =
  ((arr.(addr + 1) land 0xFF) lsl 8) lor (arr.(addr) land 0xFF)

let write_byte_array (arr : int array) (addr : int) (value : int) : unit =
  arr.(addr) <- value land 0xFF

let write_halfword_array (arr : int array) (addr : int) (value : int) : unit =
  arr.(addr) <- value land 0xFF;
  arr.(addr + 1) <- (value lsr 8) land 0xFF

let cache_isolated cpu = cpu.cp0.sr land 0x10000 <> 0
let cache_addr addr = addr land 0xFFF

let read_word_cache cache addr =
  let a = cache_addr addr in
  ((cache.(a + 3) land 0xFF) lsl 24)
  lor ((cache.(a + 2) land 0xFF) lsl 16)
  lor ((cache.(a + 1) land 0xFF) lsl 8)
  lor (cache.(a) land 0xFF)

let write_word_cache cache addr value =
  let a = cache_addr addr in
  cache.(a) <- value land 0xFF;
  cache.(a + 1) <- (value lsr 8) land 0xFF;
  cache.(a + 2) <- (value lsr 16) land 0xFF;
  cache.(a + 3) <- (value lsr 24) land 0xFF

let read_byte_cache cache addr =
  let b = cache.(cache_addr addr) land 0xFF in
  if b land 0x80 <> 0 then b lor lnot 0xFF else b

let read_byte_u_cache cache addr = cache.(cache_addr addr) land 0xFF

let read_halfword_cache cache addr =
  let a = cache_addr addr in
  let h = ((cache.(a + 1) land 0xFF) lsl 8) lor (cache.(a) land 0xFF) in
  if h land 0x8000 <> 0 then h lor lnot 0xFFFF else h

let read_halfword_u_cache cache addr =
  let a = cache_addr addr in
  ((cache.(a + 1) land 0xFF) lsl 8) lor (cache.(a) land 0xFF)

let write_byte_cache cache addr value =
  cache.(cache_addr addr) <- value land 0xFF

let write_halfword_cache cache addr value =
  let a = cache_addr addr in
  cache.(a) <- value land 0xFF;
  cache.(a + 1) <- (value lsr 8) land 0xFF

let fetch_word (cpu : cpu) (addr : int) : int =
  let p = phys_addr addr in
  if p >= 0x1FC00000 && p < 0x1FC80000 then
    read_word_array cpu.bios (p - 0x1FC00000)
  else if p >= 0x1F800000 && p < 0x1F800400 then
    read_word_array cpu.scratchpad (p - 0x1F800000)
  else if p >= 0 && p < 0x00200000 then read_word_array cpu.ram p
  else 0

let gpu_status () = 0x1C000000

let read_word (cpu : cpu) (addr : int) : int =
  if cache_isolated cpu then read_word_cache cpu.cache addr
  else
    let p = phys_addr addr in
    if p >= 0x1FC00000 && p < 0x1FC80000 then
      read_word_array cpu.bios (p - 0x1FC00000)
    else if p >= 0x1F800000 && p < 0x1F800400 then
      read_word_array cpu.scratchpad (p - 0x1F800000)
    else if p >= 0 && p < 0x00200000 then read_word_array cpu.ram p
    else if p = 0x1F801810 || p = 0x1F801814 then gpu_status ()
    else if p = 0x1F801070 then cpu.i_stat
    else if p = 0x1F801074 then cpu.i_mask
    else 0

let write_word (cpu : cpu) (addr : int) (value : int) : unit =
  if cache_isolated cpu then write_word_cache cpu.cache addr value
  else
    let p = phys_addr addr in
    if p >= 0x1FC00000 && p < 0x1FC80000 then
      write_word_array cpu.bios (p - 0x1FC00000) value
    else if p >= 0x1F800000 && p < 0x1F800400 then
      write_word_array cpu.scratchpad (p - 0x1F800000) value
    else if p >= 0 && p < 0x00200000 then write_word_array cpu.ram p value
    else if p = 0x1F801070 then cpu.i_stat <- cpu.i_stat land lnot value
    else if p = 0x1F801074 then cpu.i_mask <- value land 0x7FF
    else ()

let read_byte (cpu : cpu) (addr : int) : int =
  if cache_isolated cpu then read_byte_cache cpu.cache addr
  else
    let p = phys_addr addr in
    if p >= 0x1FC00000 && p < 0x1FC80000 then
      read_byte_array cpu.bios (p - 0x1FC00000)
    else if p >= 0x1F800000 && p < 0x1F800400 then
      read_byte_array cpu.scratchpad (p - 0x1F800000)
    else if p >= 0 && p < 0x00200000 then read_byte_array cpu.ram p
    else 0

let read_byte_u (cpu : cpu) (addr : int) : int =
  if cache_isolated cpu then read_byte_u_cache cpu.cache addr
  else
    let p = phys_addr addr in
    if p >= 0x1FC00000 && p < 0x1FC80000 then
      read_byte_u_array cpu.bios (p - 0x1FC00000)
    else if p >= 0x1F800000 && p < 0x1F800400 then
      read_byte_u_array cpu.scratchpad (p - 0x1F800000)
    else if p >= 0 && p < 0x00200000 then read_byte_u_array cpu.ram p
    else 0

let read_halfword (cpu : cpu) (addr : int) : int =
  if cache_isolated cpu then read_halfword_cache cpu.cache addr
  else
    let p = phys_addr addr in
    if p >= 0x1FC00000 && p < 0x1FC80000 then
      read_halfword_array cpu.bios (p - 0x1FC00000)
    else if p >= 0x1F800000 && p < 0x1F800400 then
      read_halfword_array cpu.scratchpad (p - 0x1F800000)
    else if p >= 0 && p < 0x00200000 then read_halfword_array cpu.ram p
    else 0

let read_halfword_u (cpu : cpu) (addr : int) : int =
  if cache_isolated cpu then read_halfword_u_cache cpu.cache addr
  else
    let p = phys_addr addr in
    if p >= 0x1FC00000 && p < 0x1FC80000 then
      read_halfword_u_array cpu.bios (p - 0x1FC00000)
    else if p >= 0x1F800000 && p < 0x1F800400 then
      read_halfword_u_array cpu.scratchpad (p - 0x1F800000)
    else if p >= 0 && p < 0x00200000 then read_halfword_u_array cpu.ram p
    else 0

let write_byte (cpu : cpu) (addr : int) (value : int) : unit =
  if cache_isolated cpu then write_byte_cache cpu.cache addr value
  else
    let p = phys_addr addr in
    if p >= 0x1FC00000 && p < 0x1FC80000 then
      write_byte_array cpu.bios (p - 0x1FC00000) value
    else if p >= 0x1F800000 && p < 0x1F800400 then
      write_byte_array cpu.scratchpad (p - 0x1F800000) value
    else if p >= 0 && p < 0x00200000 then write_byte_array cpu.ram p value
    else ()

let write_halfword (cpu : cpu) (addr : int) (value : int) : unit =
  if cache_isolated cpu then write_halfword_cache cpu.cache addr value
  else
    let p = phys_addr addr in
    if p >= 0x1FC00000 && p < 0x1FC80000 then
      write_halfword_array cpu.bios (p - 0x1FC00000) value
    else if p >= 0x1F800000 && p < 0x1F800400 then
      write_halfword_array cpu.scratchpad (p - 0x1F800000) value
    else if p >= 0 && p < 0x00200000 then write_halfword_array cpu.ram p value
    else ()

let mask16 imm = imm land 0xFFFF

let to32 v =
  let masked = v land 0xFFFFFFFF in
  if masked land 0x80000000 <> 0 then masked lor lnot 0xFFFFFFFF else masked

let ext16 v = if v land 0x8000 <> 0 then v lor lnot 0xFFFF else v land 0xFFFF

let ext32 v =
  let masked = v land 0xFFFFFFFF in
  if masked land 0x80000000 <> 0 then masked lor lnot 0xFFFFFFFF else masked

let eq32 : int -> int -> bool = fun a b -> to32 a = to32 b
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

let divu_op a b =
  let a_val = a land 0xFFFFFFFF in
  let b_val = b land 0xFFFFFFFF in
  if b_val = 0 then (a_val, -1) else (a_val mod b_val, a_val / b_val)

let div_op =
 fun a b ->
  (* from jsgroth
    this is how DIV and DIVU behave when the divisor is 0:

    Remainder is always set to the dividend
    DIV with a non-negative dividend and DIVU set the quotient to $FFFFFFFF (-1)
    DIV with a negative dividend sets the quotient to $00000001 (+1)
  *)
  let a_val = ext32 a in
  let b_val = ext32 b in
  if b_val = 0 then if a_val >= 0 then (a_val, -1) else (a_val, 1)
  else if a_val = -0x80000000 && b_val = -1 then (0, -0x80000000)
  else (a_val mod b_val, a_val / b_val)

let handle_bios_call (cpu : cpu) =
  if
    (cpu.pc = 0xA0 && cpu.regs.gp.(9) = 0x3C)
    || (cpu.pc = 0xB0 && cpu.regs.gp.(9) = 0x3D)
  then (
    let char_code = cpu.regs.gp.(4) land 0xFF in
    print_char (Char.chr char_code);
    flush stdout)

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
    if reg_num = 0 then () (* noop *) else cpu.regs.gp.(reg_num) <- to32 value
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
    | 7 -> cpu.cp0.reserved7
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
    | 7 -> cpu.cp0.reserved7 <- value
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

  let next_pc, in_delay_slot, branch_pc =
    match cpu.regs.delayed_branch with
    | Some (target, branch_pc) ->
        cpu.regs.delayed_branch <- None;
        (target, true, Some branch_pc)
    | None -> (cpu.pc + 4, false, None)
  in

  let raise_exception (exc : cpu_exception) : unit =
    let code = code_of_exception exc in
    (* If the exception occurred in a branch delay slot, EPC points to the
       branch instruction and Cause.BD is set. *)
    cpu.cp0.epc <-
      (match branch_pc with Some pc when in_delay_slot -> pc | _ -> cpu.pc);
    let bd_mask = 1 lsl 31 in
    cpu.cp0.cause <-
      (if in_delay_slot then cpu.cp0.cause lor bd_mask
       else cpu.cp0.cause land lnot bd_mask);

    (* TODO verify this cause clanker said it *)
    let cause_exccode_mask = lnot 0x7C in
    cpu.cp0.cause <- cpu.cp0.cause land cause_exccode_mask lor (code lsl 2);

    (* Reflect pending hardware interrupts in Cause.IP2 (bit 10). *)
    let pending = cpu.i_stat land cpu.i_mask in
    let ip2_mask = 1 lsl 10 in
    cpu.cp0.cause <-
      (if pending <> 0 then cpu.cp0.cause lor ip2_mask
       else cpu.cp0.cause land lnot ip2_mask);

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

  (* Check for pending hardware interrupts at instruction boundaries. *)
  let check_interrupts () =
    if not in_delay_slot then
      let pending = cpu.i_stat land cpu.i_mask land 0x7FF in
      if pending <> 0 && cpu.cp0.sr land 1 <> 0 then raise_exception Interrupt
  in
  let load_word_helper rt rs imm op is_right =
    let addr = get_reg rs + ext16 imm in
    let word_addr = addr land lnot 3 in
    let word = read_word cpu word_addr in
    let shift = if is_right then addr land 3 * 8 else (3 - (addr land 3)) * 8 in
    let mask = op 0xFFFFFFFF shift land 0xFFFFFFFF in
    let new_value = op word shift land mask in
    let old_value = get_reg rt land lnot mask in
    set_reg rt (new_value lor old_value)
  in

  let store_word_helper rt rs imm op is_right =
    let addr = get_reg rs + ext16 imm in
    let word_addr = addr land lnot 3 in
    let mem_word = read_word cpu word_addr in
    let reg_val = get_reg rt in
    let shift_right =
      if is_right then addr land 3 * 8 else (3 - (addr land 3)) * 8
    in
    let new_bits_mask =
      if is_right then 0xFFFFFFFF lsl shift_right
      else 0xFFFFFFFF lsr shift_right
    in
    let new_mem_chunk =
      if is_right then (reg_val lsl shift_right) land new_bits_mask
      else (reg_val lsr shift_right) land new_bits_mask
    in
    let old_mem_chunk = mem_word land lnot new_bits_mask in
    write_word cpu word_addr (new_mem_chunk lor old_mem_chunk)
  in

  try
    check_interrupts ();
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
    | NOR (rd, rs, rt) -> exec_rtype (fun a b -> lnot (a lor b)) no_ovf rd rs rt
    | XOR (rd, rs, rt) -> exec_rtype ( lxor ) no_ovf rd rs rt
    | XORI (rt, rs, imm) -> exec_itype ( lxor ) no_ovf rt rs (mask16 imm)
    | SLT (rd, rs, rt) ->
        let res = if ext32 (get_reg rs) < ext32 (get_reg rt) then 1 else 0 in
        set_reg rd res
    | SLTU (rd, rs, rt) ->
        let res =
          if get_reg rs land 0xFFFFFFFF < get_reg rt land 0xFFFFFFFF then 1
          else 0
        in
        set_reg rd res
    | SLTI (rt, rs, imm) ->
        let res = if ext32 (get_reg rs) < ext16 imm then 1 else 0 in
        set_reg rt res
    | SLTIU (rt, rs, imm) ->
        let res =
          if get_reg rs land 0xFFFFFFFF < ext16 imm land 0xFFFFFFFF then 1
          else 0
        in
        set_reg rt res
    | SLL (rd, rt, shamt) -> set_reg rd ((get_reg rt lsl shamt) land 0xFFFFFFFF)
    | SRL (rd, rt, shamt) -> set_reg rd ((get_reg rt land 0xFFFFFFFF) lsr shamt)
    | SRA (rd, rt, shamt) ->
        let v = ext32 (get_reg rt) in
        set_reg rd (v asr shamt)
    | SLLV (rd, rt, rs) ->
        let shamt = get_reg rs land 0x1F in
        set_reg rd ((get_reg rt lsl shamt) land 0xFFFFFFFF)
    | SRLV (rd, rt, rs) ->
        let shamt = get_reg rs land 0x1F in
        set_reg rd ((get_reg rt land 0xFFFFFFFF) lsr shamt)
    | SRAV (rd, rt, rs) ->
        let shamt = get_reg rs land 0x1F in
        let v = ext32 (get_reg rt) in
        set_reg rd (v asr shamt)
    | LUI (rt, imm) -> set_reg rt ((imm land 0xFFFF) lsl 16)
    | LB (rt, rs, imm) -> set_reg rt (read_byte cpu (get_reg rs + ext16 imm))
    | LBU (rt, rs, imm) -> set_reg rt (read_byte_u cpu (get_reg rs + ext16 imm))
    | LH (rt, rs, imm) ->
        let addr = get_reg rs + ext16 imm in
        if addr land 1 <> 0 then raise_exception (AddressErrorLoad addr)
        else set_reg rt (read_halfword cpu addr)
    | LHU (rt, rs, imm) ->
        let addr = get_reg rs + ext16 imm in
        if addr land 1 <> 0 then raise_exception (AddressErrorLoad addr)
        else set_reg rt (read_halfword_u cpu addr)
    | LW (rt, rs, imm) ->
        let addr = get_reg rs + ext16 imm in
        if addr land 3 <> 0 then raise_exception (AddressErrorLoad addr)
        else set_reg rt (read_word cpu addr)
    | LWL (rt, rs, imm) -> load_word_helper rt rs imm ( lsl ) false
    | LWR (rt, rs, imm) -> load_word_helper rt rs imm ( lsr ) true
    | SB (rt, rs, imm) -> write_byte cpu (get_reg rs + ext16 imm) (get_reg rt)
    | SH (rt, rs, imm) ->
        let addr = get_reg rs + ext16 imm in
        if addr land 1 <> 0 then raise_exception (AddressErrorStore addr)
        else write_halfword cpu addr (get_reg rt)
    | SW (rt, rs, imm) ->
        let addr = get_reg rs + ext16 imm in
        if addr land 3 <> 0 then raise_exception (AddressErrorStore addr)
        else write_word cpu addr (get_reg rt)
    | SWL (rt, rs, imm) -> store_word_helper rt rs imm ( lsl ) false
    | SWR (rt, rs, imm) -> store_word_helper rt rs imm ( lsr ) true
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
        cpu.regs.delayed_branch <- Some (target_addr, cpu.pc)
    | JAL target ->
        set_reg 31 (cpu.pc + 8);
        let target_addr = cpu.pc land 0xF0000000 lor (target lsl 2) in
        cpu.regs.delayed_branch <- Some (target_addr, cpu.pc)
    | JALR (rd, rs) ->
        set_reg rd (cpu.pc + 8);
        cpu.regs.delayed_branch <- Some (get_reg rs, cpu.pc)
    | JR rs -> cpu.regs.delayed_branch <- Some (get_reg rs, cpu.pc)
    | RFE ->
        let mode_bits = cpu.cp0.sr land 0x3F in
        let sr_cleared = cpu.cp0.sr land lnot 0x3F in
        cpu.cp0.sr <- sr_cleared lor ((mode_bits lsr 2) land 0x3F)
    | BLTZ (rs, offset) ->
        if ext32 (get_reg rs) < 0 then
          let target_addr = cpu.pc + 4 + (ext16 offset lsl 2) in
          cpu.regs.delayed_branch <- Some (target_addr, cpu.pc)
    | BGEZ (rs, offset) ->
        if ext32 (get_reg rs) >= 0 then
          let target_addr = cpu.pc + 4 + (ext16 offset lsl 2) in
          cpu.regs.delayed_branch <- Some (target_addr, cpu.pc)
    | BLTZAL (rs, offset) ->
        set_reg 31 (cpu.pc + 8);
        if ext32 (get_reg rs) < 0 then
          let target_addr = cpu.pc + 4 + (ext16 offset lsl 2) in
          cpu.regs.delayed_branch <- Some (target_addr, cpu.pc)
    | BGEZAL (rs, offset) ->
        set_reg 31 (cpu.pc + 8);
        if ext32 (get_reg rs) >= 0 then
          let target_addr = cpu.pc + 4 + (ext16 offset lsl 2) in
          cpu.regs.delayed_branch <- Some (target_addr, cpu.pc)
    | BEQ (rs, rt, offset) ->
        if get_reg rs = get_reg rt then
          let target_addr = cpu.pc + 4 + (ext16 offset lsl 2) in
          cpu.regs.delayed_branch <- Some (target_addr, cpu.pc)
    | BNE (rs, rt, offset) ->
        if get_reg rs <> get_reg rt then
          let target_addr = cpu.pc + 4 + (ext16 offset lsl 2) in
          cpu.regs.delayed_branch <- Some (target_addr, cpu.pc)
    | BGTZ (rs, offset) ->
        if ext32 (get_reg rs) > 0 then
          let target_addr = cpu.pc + 4 + (ext16 offset lsl 2) in
          cpu.regs.delayed_branch <- Some (target_addr, cpu.pc)
    | BLEZ (rs, offset) ->
        if ext32 (get_reg rs) <= 0 then
          let target_addr = cpu.pc + 4 + (ext16 offset lsl 2) in
          cpu.regs.delayed_branch <- Some (target_addr, cpu.pc)
    | MOVZ (rd, rs, rt) -> if get_reg rt = 0 then set_reg rd (get_reg rs)
    | MOVN (rd, rs, rt) -> if get_reg rt <> 0 then set_reg rd (get_reg rs));
    cpu.pc <- next_pc
  with CpuException _ -> ()

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
  if len >= width then raw else String.make (width - len) '0' ^ raw

let () =
  Printexc.register_printer (function
    | UnknownOpcode code -> Some (Printf.sprintf "Unknown Opcode: 0x%08X" code)
    | UnknownFunction funct ->
        Some
          (Printf.sprintf "Unknown Function: %s" (string_of_bin ~width:6 funct))
    | _ -> None)

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
      | 0b001010 -> MOVZ (rd, rs, rt)
      | 0b001011 -> MOVN (rd, rs, rt)
      | 0b011000 -> MULT (rs, rt)
      | 0b011001 -> MULTU (rs, rt)
      | 0b011010 -> DIV (rs, rt)
      | 0b011011 -> DIVU (rs, rt)
      | 0b010000 -> MFHI rd
      | 0b010010 -> MFLO rd
      | 0b010001 -> MTHI rs
      | 0b010011 -> MTLO rs
      | 0b000000 -> SLL (rd, rt, shamt)
      | 0b000010 -> SRL (rd, rt, shamt)
      | 0b000011 -> SRA (rd, rt, shamt)
      | 0b000100 -> SLLV (rd, rt, rs)
      | 0b000110 -> SRLV (rd, rt, rs)
      | 0b000111 -> SRAV (rd, rt, rs)
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
  | 0b100010 -> LWL (rt, rs, imm)
  | 0b100110 -> LWR (rt, rs, imm)
  | 0b101000 -> SB (rt, rs, imm)
  | 0b101001 -> SH (rt, rs, imm)
  | 0b101011 -> SW (rt, rs, imm)
  | 0b101010 -> SWL (rt, rs, imm)
  | 0b101110 -> SWR (rt, rs, imm)
  | 0b000010 -> J target
  | 0b000011 -> JAL target
  | 0b000100 -> BEQ (rs, rt, imm)
  | 0b000101 -> BNE (rs, rt, imm)
  | 0b000111 -> BGTZ (rs, imm)
  | 0b000110 -> BLEZ (rs, imm)
  | 0b000001 -> (
      match rt with
      | 0x10 -> BLTZAL (rs, imm)
      | 0x11 -> BGEZAL (rs, imm)
      | _ -> if rt land 1 = 0 then BLTZ (rs, imm) else BGEZ (rs, imm))
  | 0b010000 -> (
      match rs with
      | 0b00000 -> MFC0 (rt, rd)
      | 0b00100 -> MTC0 (rt, rd)
      | 0b10000 -> RFE
      | _ -> raise (UnknownOpcode instr))
  | _ -> raise (UnknownOpcode instr)

let step_count = ref 0

(* let ram_dumped = ref false

let dump_ram cpu =
  let oc = open_out_bin "/tmp/camlstation_ram.bin" in
  for i = 0 to Array.length cpu.ram - 1 do
    output_byte oc (cpu.ram.(i) land 0xFF)
  done;
  close_out oc
*)
let vblank_cycles = 50000

let sideload_exe (cpu : cpu) : unit =
  let exe_path = "./psxtest_cpu.exe" in
  let exe_file = open_in_bin exe_path in
  let exe_size = in_channel_length exe_file in
  let exe_data = really_input_string exe_file exe_size in
  close_in exe_file;

  let b = Bytes.of_string exe_data in
  let initial_pc = Bytes.get_int32_le b 0x10 |> Int32.to_int in
  let initial_r28 = Bytes.get_int32_le b 0x14 |> Int32.to_int in
  let exe_ram_addr =
    (Bytes.get_int32_le b 0x18 |> Int32.to_int) land 0x1FFFFF
  in
  let exe_payload_size = Bytes.get_int32_le b 0x1C |> Int32.to_int in
  let initial_sp = Bytes.get_int32_le b 0x30 |> Int32.to_int in

  let available = exe_size - 2048 in
  let payload_size = min exe_payload_size available in
  if payload_size < 0 then failwith "Invalid EXE payload size";

  Printf.printf "[DEBUG] Sideloading EXE: PC=0x%08X RAM=0x%08X size=%d\n%!"
    initial_pc exe_ram_addr payload_size;

  let payload = String.sub exe_data 2048 payload_size in
  for i = 0 to payload_size - 1 do
    cpu.ram.(exe_ram_addr + i) <- Char.code payload.[i]
  done;

  cpu.regs.gp.(28) <- initial_r28;
  if initial_sp <> 0 then (
    cpu.regs.gp.(29) <- initial_sp;
    cpu.regs.gp.(30) <- initial_sp);
  cpu.pc <- initial_pc

let sideloaded = ref false

let step (cpu : cpu) : unit =
  incr step_count;
  cpu.cycle_count <- cpu.cycle_count + 1;
  if cpu.cycle_count mod vblank_cycles = 0 then (
    cpu.i_stat <- cpu.i_stat lor 1;
    (* kimi 2.7 helped me with this... could be wrong tho*)
    (* Simulate the BIOS VBlank interrupt handler: bump the kernel VSync
       counter.  The kernel accesses this via LUI 0x8008 + offset 0x9D9C,
       which sign-extends to physical address 0x79D9C. *)
    let cur = read_word_array cpu.ram 0x79D9C land 0xFFFFFFFF in
    write_word_array cpu.ram 0x79D9C ((cur + 1) land 0xFFFFFFFF));
  if eq32 cpu.pc 0x80030000 && not !sideloaded then (
    sideloaded := true;
    Printf.printf
      "[DEBUG] Reached shell entry PC=0x%08X\n[DEBUG] Sideloading EXE\n%!"
      cpu.pc;
    sideload_exe cpu);

  let opcode = fetch_word cpu cpu.pc in
  let instr = parse_opcode opcode in
  execute cpu instr;
  handle_bios_call cpu
