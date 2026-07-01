type dma_direction = DmaOff | DmaFifo | DmaCpuToGp0 | DmaGpuReadToCpu

type renderer_command_name =
  | CmdFill
  | CmdRect
  | CmdLineFlat
  | CmdLineShaded
  | CmdVramCopy
  | CmdImageBegin
  | CmdImageWord
  | CmdDisplayReset
  | CmdDisplayArea
  | CmdDisplayHRange
  | CmdDisplayVRange
  | CmdDisplayMode

let renderer_command_name_to_string = function
  | CmdFill -> "fill"
  | CmdRect -> "rect"
  | CmdLineFlat -> "line_flat"
  | CmdLineShaded -> "line_shaded"
  | CmdVramCopy -> "vram_copy"
  | CmdImageBegin -> "image_begin"
  | CmdImageWord -> "image_word"
  | CmdDisplayReset -> "display_reset"
  | CmdDisplayArea -> "display_area"
  | CmdDisplayHRange -> "display_h_range"
  | CmdDisplayVRange -> "display_v_range"
  | CmdDisplayMode -> "display_mode"

external renderer_submit_named : string -> int -> int -> int -> int -> unit
  = "renderer_submit_named"

external renderer_submit_polygon_flat : int * int * int * int * int -> unit
  = "renderer_submit_polygon_flat"

external renderer_submit_polygon_shaded :
  int * int * int * int * int * int * int -> unit
  = "renderer_submit_polygon_shaded"

external renderer_submit_polygon_flat_quad :
  int * int * int * int * int * int -> unit
  = "renderer_submit_polygon_flat_quad"

external renderer_submit_polygon_shaded_quad :
  int * int * int * int * int * int * int * int * int -> unit
  = "renderer_submit_polygon_shaded_quad"

external renderer_submit_draw_area_top_left : int -> unit
  = "renderer_submit_draw_area_top_left"

external renderer_submit_draw_area_bottom_right : int -> unit
  = "renderer_submit_draw_area_bottom_right"

external renderer_submit_draw_mode : int -> unit = "renderer_submit_draw_mode"

let renderer_submit (cmd : renderer_command_name) (a0 : int) (a1 : int)
    (a2 : int) (a3 : int) : unit =
  renderer_submit_named (renderer_command_name_to_string cmd) a0 a1 a2 a3

type gp0_command =
  | Gp0Unknown
  | Gp0FillVram
  | Gp0CpuToVram
  | Gp0VramToVram
  | Gp0RectVar
  | Gp0RectDot
  | Gp0Rect8x8
  | Gp0Rect16x16
  | Gp0LineFlat
  | Gp0LineFlatPolyline
  | Gp0LineShaded
  | Gp0LineShadedPolyline

type gp0_state = {
  mutable first_word : int;
  mutable words_expected : int;
  mutable args_rev : int list;
  mutable image_load_active : bool;
  mutable image_words_remaining : int;
  mutable image_wh : int;
  mutable polyline_active : bool;
  mutable polyline_shaded : bool;
  mutable polyline_expect_coord : bool;
  mutable polyline_last_color : int;
  mutable polyline_next_color : int;
  mutable polyline_last_xy : int;
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
    gp0 =
      {
        first_word = 0;
        words_expected = 0;
        args_rev = [];
        image_load_active = false;
        image_words_remaining = 0;
        image_wh = 0;
        polyline_active = false;
        polyline_shaded = false;
        polyline_expect_coord = false;
        polyline_last_color = 0;
        polyline_next_color = 0;
        polyline_last_xy = 0;
      };
  }

let reset_gp0_state (gpu : gpu) : unit =
  gpu.gp0.first_word <- 0;
  gpu.gp0.words_expected <- 0;
  gpu.gp0.args_rev <- [];
  gpu.gp0.image_load_active <- false;
  gpu.gp0.image_words_remaining <- 0;
  gpu.gp0.image_wh <- 0;
  gpu.gp0.polyline_active <- false;
  gpu.gp0.polyline_shaded <- false;
  gpu.gp0.polyline_expect_coord <- false;
  gpu.gp0.polyline_last_color <- 0;
  gpu.gp0.polyline_next_color <- 0;
  gpu.gp0.polyline_last_xy <- 0

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

let gp0_is_polygon_command (opcode : int) : bool = opcode land 0xE0 = 0x20

let gp0_polygon_is_shaded (opcode : int) : bool = opcode land 0x10 <> 0

let gp0_polygon_is_quad (opcode : int) : bool = opcode land 0x08 <> 0

let gp0_polygon_is_semitransparent (opcode : int) : bool = opcode land 0x02 <> 0

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
  match
    if gp0_is_polygon_command opcode then Gp0Unknown
    else if opcode = 0x02 then Gp0FillVram
    else if opcode = 0xA0 then Gp0CpuToVram
    else if opcode land 0xE0 = 0x80 then Gp0VramToVram
    else if opcode land 0xF8 = 0x60 then Gp0RectVar
    else if opcode land 0xF8 = 0x68 then Gp0RectDot
    else if opcode land 0xF8 = 0x70 then Gp0Rect8x8
    else if opcode land 0xF8 = 0x78 then Gp0Rect16x16
    else if opcode land 0xF8 = 0x40 then Gp0LineFlat
    else if opcode land 0xF8 = 0x48 then Gp0LineFlatPolyline
    else if opcode land 0xF8 = 0x50 then Gp0LineShaded
    else if opcode land 0xF8 = 0x58 then Gp0LineShadedPolyline
    else Gp0Unknown
  with
  | Gp0FillVram | Gp0CpuToVram | Gp0RectVar | Gp0LineFlat | Gp0LineFlatPolyline
    ->
      2
  | Gp0LineShaded | Gp0LineShadedPolyline | Gp0VramToVram -> 3
  | Gp0RectDot | Gp0Rect8x8 | Gp0Rect16x16 -> 1
  | Gp0Unknown when gp0_is_polygon_command opcode ->
      if gp0_polygon_is_shaded opcode then
        if gp0_polygon_is_quad opcode then 7 else 5
      else if gp0_polygon_is_quad opcode then 4
      else 3
  | Gp0Unknown -> 0

let gp0_decode_command (opcode : int) : gp0_command =
  if opcode = 0x02 then Gp0FillVram
  else if opcode = 0xA0 then Gp0CpuToVram
  else if opcode land 0xE0 = 0x80 then Gp0VramToVram
  else if opcode land 0xF8 = 0x60 then Gp0RectVar
  else if opcode land 0xF8 = 0x68 then Gp0RectDot
  else if opcode land 0xF8 = 0x70 then Gp0Rect8x8
  else if opcode land 0xF8 = 0x78 then Gp0Rect16x16
  else if opcode land 0xF8 = 0x40 then Gp0LineFlat
  else if opcode land 0xF8 = 0x48 then Gp0LineFlatPolyline
  else if opcode land 0xF8 = 0x50 then Gp0LineShaded
  else if opcode land 0xF8 = 0x58 then Gp0LineShadedPolyline
  else Gp0Unknown

let rgb24_to_rgb555 (rgb : int) : int =
  let r8 = rgb land 0xFF in
  let g8 = (rgb lsr 8) land 0xFF in
  let b8 = (rgb lsr 16) land 0xFF in
  let r5 = ((r8 * 31) + 127) / 255 in
  let g5 = ((g8 * 31) + 127) / 255 in
  let b5 = ((b8 * 31) + 127) / 255 in
  r5 lor (g5 lsl 5) lor (b5 lsl 10)

let gp0_decode_color555 (word : int) : int =
  rgb24_to_rgb555 (word land 0x00FFFFFF)

let gp0_is_polygon_command (opcode : int) : bool = opcode land 0xE0 = 0x20

let gp0_polygon_is_shaded (opcode : int) : bool = opcode land 0x10 <> 0

let gp0_polygon_is_quad (opcode : int) : bool = opcode land 0x08 <> 0

let gp0_polygon_is_semitransparent (opcode : int) : bool = opcode land 0x02 <> 0

let gp0_is_polyline_terminator (word : int) : bool =
  word land 0xF000F000 = 0x50005000

let gp0_begin_image_load (gpu : gpu) (arg0 : int) (arg1 : int) : unit =
  let w = arg1 land 0xFFFF in
  let h = (arg1 lsr 16) land 0xFFFF in
  if w <= 0 || h <= 0 then (
    gpu.gp0.image_load_active <- false;
    gpu.gp0.image_words_remaining <- 0)
  else
    let total_pixels = w * h in
    gpu.gp0.image_words_remaining <- (total_pixels + 1) / 2;
    gpu.gp0.image_wh <- arg1;
    gpu.gp0.image_load_active <- gpu.gp0.image_words_remaining > 0;
    renderer_submit CmdImageBegin arg0 arg1 0 0

let gp0_execute_command (gpu : gpu) (first_word : int) (args : int list) : unit
    =
  let opcode = (first_word lsr 24) land 0xFF in
  match opcode with
  | 0x00 -> ()
  | 0x01 -> ()
  | 0x1F -> gpu.irq <- true
  | 0xE1 ->
      gpu.draw_mode <- first_word land 0x7FF;
      renderer_submit_draw_mode gpu.draw_mode
  | 0xE2 -> gpu.texture_window <- first_word land 0xFFFFF
  | 0xE3 ->
      gpu.drawing_area_tl <- first_word land 0x000FFFFF;
      renderer_submit_draw_area_top_left gpu.drawing_area_tl
  | 0xE4 ->
      gpu.drawing_area_br <- first_word land 0x000FFFFF;
      renderer_submit_draw_area_bottom_right gpu.drawing_area_br
  | 0xE5 -> gpu.drawing_offset <- first_word land 0x003FFFFF
  | 0xE6 -> gpu.mask_bit_setting <- first_word land 0x3
  | _ ->
      if gp0_is_polygon_command opcode then
        let semi = if gp0_polygon_is_semitransparent opcode then 1 else 0 in
        if gp0_polygon_is_shaded opcode then
          match (gp0_polygon_is_quad opcode, args) with
          | false, [ arg0; arg1; arg2; arg3; arg4 ] ->
              renderer_submit_polygon_shaded
                ( semi,
                  gp0_decode_color555 first_word,
                  arg0,
                  gp0_decode_color555 arg1,
                  arg2,
                  gp0_decode_color555 arg3,
                  arg4 )
          | true, [ arg0; arg1; arg2; arg3; arg4; arg5; arg6 ] ->
              renderer_submit_polygon_shaded_quad
                ( semi,
                  gp0_decode_color555 first_word,
                  arg0,
                  gp0_decode_color555 arg1,
                  arg2,
                  gp0_decode_color555 arg3,
                  arg4,
                  gp0_decode_color555 arg5,
                  arg6 )
          | _ -> ()
        else
          match (gp0_polygon_is_quad opcode, args) with
          | false, [ arg0; arg1; arg2 ] ->
              renderer_submit_polygon_flat
                (semi, gp0_decode_color555 first_word, arg0, arg1, arg2)
          | true, [ arg0; arg1; arg2; arg3 ] ->
              renderer_submit_polygon_flat_quad
                (semi, gp0_decode_color555 first_word, arg0, arg1, arg2, arg3)
          | _ -> ()
      else
        match (gp0_decode_command opcode, args) with
        | Gp0FillVram, [ arg0; arg1 ] ->
            renderer_submit CmdFill (first_word land 0x00FFFFFF) arg0 arg1 0
        | Gp0CpuToVram, [ arg0; arg1 ] -> gp0_begin_image_load gpu arg0 arg1
        | Gp0VramToVram, [ src_xy; dst_xy; wh ] ->
            renderer_submit CmdVramCopy src_xy dst_xy wh 0
        | Gp0RectVar, [ arg0; arg1 ] ->
            renderer_submit CmdRect (first_word land 0x00FFFFFF) arg0 arg1 0
        | Gp0RectDot, [ arg0 ] ->
            renderer_submit CmdRect
              (first_word land 0x00FFFFFF)
              arg0
              ((1 lsl 16) lor 1)
              0
        | Gp0Rect8x8, [ arg0 ] ->
            renderer_submit CmdRect
              (first_word land 0x00FFFFFF)
              arg0
              ((8 lsl 16) lor 8)
              0
        | Gp0Rect16x16, [ arg0 ] ->
            renderer_submit CmdRect
              (first_word land 0x00FFFFFF)
              arg0
              ((16 lsl 16) lor 16)
              0
        | Gp0LineFlat, [ arg0; arg1 ] ->
            renderer_submit CmdLineFlat
              (gp0_decode_color555 first_word)
              arg0 arg1 0
        | Gp0LineFlatPolyline, [ arg0; arg1 ] ->
            renderer_submit CmdLineFlat
              (gp0_decode_color555 first_word)
              arg0 arg1 0;
            gpu.gp0.polyline_active <- true;
            gpu.gp0.polyline_shaded <- false;
            gpu.gp0.polyline_expect_coord <- false;
            gpu.gp0.polyline_last_color <- gp0_decode_color555 first_word;
            gpu.gp0.polyline_last_xy <- arg1
        | Gp0LineShaded, [ arg0; arg1_color; arg2 ] ->
            renderer_submit CmdLineShaded
              (gp0_decode_color555 first_word)
              (gp0_decode_color555 arg1_color)
              arg0 arg2
        | Gp0LineShadedPolyline, [ arg0; arg1_color; arg2 ] ->
            renderer_submit CmdLineShaded
              (gp0_decode_color555 first_word)
              (gp0_decode_color555 arg1_color)
              arg0 arg2;
            gpu.gp0.polyline_active <- true;
            gpu.gp0.polyline_shaded <- true;
            gpu.gp0.polyline_expect_coord <- false;
            gpu.gp0.polyline_last_color <- gp0_decode_color555 arg1_color;
            gpu.gp0.polyline_last_xy <- arg2
          | _ -> ()

let process_gp0_polyline_word (gpu : gpu) (word : int) : unit =
  if gp0_is_polyline_terminator word then (
    gpu.gp0.polyline_active <- false;
    gpu.gp0.polyline_shaded <- false;
    gpu.gp0.polyline_expect_coord <- false)
  else if not gpu.gp0.polyline_shaded then (
    renderer_submit CmdLineFlat gpu.gp0.polyline_last_color
      gpu.gp0.polyline_last_xy word 0;
    gpu.gp0.polyline_last_xy <- word)
  else if not gpu.gp0.polyline_expect_coord then (
    gpu.gp0.polyline_next_color <- gp0_decode_color555 word;
    gpu.gp0.polyline_expect_coord <- true)
  else (
    renderer_submit CmdLineShaded gpu.gp0.polyline_last_color
      gpu.gp0.polyline_next_color gpu.gp0.polyline_last_xy word;
    gpu.gp0.polyline_last_xy <- word;
    gpu.gp0.polyline_last_color <- gpu.gp0.polyline_next_color;
    gpu.gp0.polyline_expect_coord <- false)

let write_gp0 (gpu : gpu) (value : int) : unit =
  if gpu.gp0.image_load_active then (
    renderer_submit CmdImageWord value 0 0 0;
    gpu.gp0.image_words_remaining <- gpu.gp0.image_words_remaining - 1;
    if gpu.gp0.image_words_remaining <= 0 then (
      gpu.gp0.image_words_remaining <- 0;
      gpu.gp0.image_load_active <- false))
  else if gpu.gp0.polyline_active then process_gp0_polyline_word gpu value
  else if gpu.gp0.words_expected = 0 then (
    gpu.gp0.first_word <- value;
    gpu.gp0.args_rev <- [];
    let opcode = (value lsr 24) land 0xFF in
    gpu.gp0.words_expected <- gp0_param_words opcode;
    if gpu.gp0.words_expected = 0 then gp0_execute_command gpu value [])
  else (
    gpu.gp0.args_rev <- value :: gpu.gp0.args_rev;
    gpu.gp0.words_expected <- gpu.gp0.words_expected - 1;
    if gpu.gp0.words_expected = 0 then (
      gp0_execute_command gpu gpu.gp0.first_word (List.rev gpu.gp0.args_rev);
      gpu.gp0.args_rev <- []))

let write_gp1 (gpu : gpu) (value : int) : unit =
  let opcode = (value lsr 24) land 0xFF in
  match opcode with
  | 0x00 ->
      reset gpu;
      renderer_submit CmdDisplayReset 0 0 0 0
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
  | 0x05 ->
      gpu.display_area <- value land 0x0007FFFF;
      renderer_submit CmdDisplayArea gpu.display_area 0 0 0
  | 0x06 ->
      gpu.display_h_range <- value land 0x000FFFFF;
      renderer_submit CmdDisplayHRange gpu.display_h_range 0 0 0
  | 0x07 ->
      gpu.display_v_range <- value land 0x000FFFFF;
      renderer_submit CmdDisplayVRange gpu.display_v_range 0 0 0
  | 0x08 ->
      gpu.display_mode <- value land 0xFF;
      renderer_submit CmdDisplayMode gpu.display_mode 0 0 0
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
