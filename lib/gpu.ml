type dma_direction = DmaOff | DmaFifo | DmaCpuToGp0 | DmaGpuReadToCpu

type gp0_state = {
  mutable first_word : int;
  mutable words_expected : int;
  mutable args_rev : int list;
}

type gpu = {
  mutable gpuread : int;
  mutable irq : bool;
  mutable display_disabled : bool;
  mutable dma_direction : dma_direction;
  mutable draw_mode : int;
  mutable texture_window : int;
  mutable drawing_area_tl : int;
  mutable drawing_area_br : int;
  mutable drawing_offset : int;
  mutable mask_bit_setting : int;
  mutable display_area : int;
  mutable display_h_range : int;
  mutable display_v_range : int;
  mutable display_mode : int;
  gp0 : gp0_state;
}

let io_gp0 = 0x1F801810
let io_gp1 = 0x1F801814
let default_gpuread = 0x1C000000

let create () =
  {
    gpuread = default_gpuread;
    irq = false;
    display_disabled = false;
    dma_direction = DmaOff;
    draw_mode = 0;
    texture_window = 0;
    drawing_area_tl = 0;
    drawing_area_br = 0;
    drawing_offset = 0;
    mask_bit_setting = 0;
    display_area = 0;
    display_h_range = 0;
    display_v_range = 0;
    display_mode = 0;
    gp0 = { first_word = 0; words_expected = 0; args_rev = [] };
  }

let reset_gp0_state (gpu : gpu) : unit =
  gpu.gp0.first_word <- 0;
  gpu.gp0.words_expected <- 0;
  gpu.gp0.args_rev <- []

let reset (gpu : gpu) : unit =
  gpu.gpuread <- default_gpuread;
  gpu.irq <- false;
  gpu.display_disabled <- false;
  gpu.dma_direction <- DmaOff;
  gpu.draw_mode <- 0;
  gpu.texture_window <- 0;
  gpu.drawing_area_tl <- 0;
  gpu.drawing_area_br <- 0;
  gpu.drawing_offset <- 0;
  gpu.mask_bit_setting <- 0;
  gpu.display_area <- 0;
  gpu.display_h_range <- 0;
  gpu.display_v_range <- 0;
  gpu.display_mode <- 0;
  reset_gp0_state gpu

let dma_direction_bits = function
  | DmaOff -> 0
  | DmaFifo -> 1
  | DmaCpuToGp0 -> 2
  | DmaGpuReadToCpu -> 3

let gpustat (gpu : gpu) : int =
  let status = ref 0 in

  (* Draw-mode related fields in low bits. *)
  status := !status lor (gpu.draw_mode land 0x7FF);

  (* Display disable flag (GP1(03)). *)
  if gpu.display_disabled then status := !status lor (1 lsl 23);

  (* IRQ request flag. *)
  if gpu.irq then status := !status lor (1 lsl 24);

  (* Command/transfer readiness: minimal model keeps GPU always ready. *)
  status := !status lor (1 lsl 26);
  status := !status lor (1 lsl 27);
  status := !status lor (1 lsl 28);

  (* DMA direction selected by GP1(04). *)
  status := !status lor (dma_direction_bits gpu.dma_direction lsl 29);

  !status

let gp0_param_words (opcode : int) : int =
  let hi3 = opcode lsr 5 in
  match hi3 with
  | 0b001 -> 0
  | 0b010 -> 0
  | 0b011 -> 0
  | 0b100 -> 3
  | 0b101 -> 2
  | 0b110 -> 2
  | _ -> (
      match opcode with
      | 0x00 -> 0
      | 0x01 -> 0
      | 0x02 -> 2
      | 0x1F -> 0
      | 0xE1 -> 0
      | 0xE2 -> 0
      | 0xE3 -> 0
      | 0xE4 -> 0
      | 0xE5 -> 0
      | 0xE6 -> 0
      | _ -> 0)

let execute_gp0_command (gpu : gpu) (first_word : int) (args : int list) : unit
    =
  let opcode = (first_word lsr 24) land 0xFF in
  match opcode with
  | 0x00 -> ()
  | 0x01 -> ()
  | 0x02 -> ()
  | 0x1F -> gpu.irq <- true
  | 0xE1 -> gpu.draw_mode <- first_word land 0x7FF
  | 0xE2 -> gpu.texture_window <- first_word land 0xFFFFF
  | 0xE3 -> gpu.drawing_area_tl <- first_word land 0x000FFFFF
  | 0xE4 -> gpu.drawing_area_br <- first_word land 0x000FFFFF
  | 0xE5 -> gpu.drawing_offset <- first_word land 0x003FFFFF
  | 0xE6 -> gpu.mask_bit_setting <- first_word land 0x3
  | _ -> ignore args

let write_gp0 (gpu : gpu) (value : int) : unit =
  if gpu.gp0.words_expected = 0 then (
    gpu.gp0.first_word <- value;
    gpu.gp0.args_rev <- [];
    let opcode = (value lsr 24) land 0xFF in
    gpu.gp0.words_expected <- gp0_param_words opcode;
    if gpu.gp0.words_expected = 0 then execute_gp0_command gpu value [])
  else (
    gpu.gp0.args_rev <- value :: gpu.gp0.args_rev;
    gpu.gp0.words_expected <- gpu.gp0.words_expected - 1;
    if gpu.gp0.words_expected = 0 then (
      execute_gp0_command gpu gpu.gp0.first_word (List.rev gpu.gp0.args_rev);
      gpu.gp0.args_rev <- []))

let write_gp1 (gpu : gpu) (value : int) : unit =
  let opcode = (value lsr 24) land 0xFF in
  match opcode with
  | 0x00 -> reset gpu
  | 0x01 -> reset_gp0_state gpu
  | 0x02 -> gpu.irq <- false
  | 0x03 -> gpu.display_disabled <- value land 1 <> 0
  | 0x04 ->
      gpu.dma_direction <-
        (match value land 0x3 with
        | 0 -> DmaOff
        | 1 -> DmaFifo
        | 2 -> DmaCpuToGp0
        | _ -> DmaGpuReadToCpu)
  | 0x05 -> gpu.display_area <- value land 0x0007FFFF
  | 0x06 -> gpu.display_h_range <- value land 0x000FFFFF
  | 0x07 -> gpu.display_v_range <- value land 0x000FFFFF
  | 0x08 -> gpu.display_mode <- value land 0xFF
  | 0x10 ->
      let reg_index = value land 0x7 in
      gpu.gpuread <-
        (match reg_index with
        | 2 -> gpu.texture_window
        | 3 -> gpu.drawing_area_tl
        | 4 -> gpu.drawing_area_br
        | 5 -> gpu.drawing_offset
        | 7 -> gpu.gpuread
        | _ -> 0)
  | _ -> ()

let read_port (gpu : gpu) (addr : int) : int option =
  match addr with
  | a when a = io_gp0 -> Some gpu.gpuread
  | a when a = io_gp1 -> Some (gpustat gpu)
  | _ -> None

let write_port (gpu : gpu) (addr : int) (value : int) : bool =
  match addr with
  | a when a = io_gp0 ->
      write_gp0 gpu value;
      true
  | a when a = io_gp1 ->
      write_gp1 gpu value;
      true
  | _ -> false
