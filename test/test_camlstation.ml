open Camlstation

let failf fmt = Printf.ksprintf failwith fmt

let assert_pixel st x y expected =
  let actual = st.Renderer.vram.((y * Renderer.vram_width) + x) in
  if actual <> expected then
    failf "pixel (%d,%d): expected %04X, got %04X" x y expected actual

let drain_renderer () =
  let st = !Renderer.current in
  while not (Queue.is_empty st.Renderer.queue) do
    Renderer.process_command st (Queue.pop st.Renderer.queue)
  done

let gp0 gpu word = Gpu.write_gp0 gpu word

let xy x y = x lor (y lsl 16)

let test_textured_rectangles () =
  let st = !Renderer.current in
  Renderer.reset_state st;
  let gpu = Gpu.create () in

  (* A 2x2 direct-color texture at the page selected by E1(0508h). *)
  gp0 gpu 0xA0000000;
  gp0 gpu 0x00000200;
  gp0 gpu 0x00020002;
  gp0 gpu 0x83E0801F;
  gp0 gpu 0xFC00FFFF;

  (* Variable-size raw textured rectangle. *)
  gp0 gpu 0xE1000508;
  gp0 gpu 0x65FFFFFF;
  gp0 gpu 0x0014000A;
  gp0 gpu 0x00000000;
  gp0 gpu 0x00020002;
  drain_renderer ();
  assert_pixel st 10 20 0x801F;
  assert_pixel st 11 20 0x83E0;
  assert_pixel st 10 21 0xFFFF;
  assert_pixel st 11 21 0xFC00;

  (* E1 bits 12/13 reverse rectangle texture stepping. *)
  gp0 gpu 0xE1003508;
  gp0 gpu 0x65FFFFFF;
  gp0 gpu 0x001E0014;
  gp0 gpu 0x00000101;
  gp0 gpu 0x00020002;

  (* Fixed-size textured packets have a UV word but no size word. Sending a
     command after one also checks that the GP0 packet parser stayed aligned. *)
  gp0 gpu 0xE1000508;
  gp0 gpu 0x7DFFFFFF;
  gp0 gpu 0x0028001E;
  gp0 gpu 0x00000000;
  gp0 gpu 0x680000FF;
  gp0 gpu 0x00320032;
  drain_renderer ();
  assert_pixel st 20 30 0xFC00;
  assert_pixel st 21 30 0xFFFF;
  assert_pixel st 20 31 0x83E0;
  assert_pixel st 21 31 0x801F;
  assert_pixel st 30 40 0x801F;
  assert_pixel st 50 50 0x001F;

  (* Modulation uses the command color; semi-transparency applies only when
     the sampled texel has bit 15 set. *)
  gp0 gpu 0x64_00FF00;
  gp0 gpu 0x003C003C;
  gp0 gpu 0x00000001;
  gp0 gpu 0x00010001;
  gp0 gpu 0x67FFFFFF;
  gp0 gpu 0x003C003D;
  gp0 gpu 0x00000000;
  gp0 gpu 0x00010001;
  drain_renderer ();
  assert_pixel st 60 60 0x83E0;
  assert_pixel st 61 60 0x800F

let test_texture_windows () =
  let st = !Renderer.current in
  Renderer.reset_state st;
  let gpu = Gpu.create () in
  for y = 0 to 31 do
    for x = 0 to 15 do
      st.Renderer.vram.((y * Renderer.vram_width) + 512 + x) <-
        0x8000 lor (x + 1) lor ((y + 1) lsl 5)
    done
  done;

  (* Mask bit 0 repeats U every 8 texels; mask bit 1 of V repeats every 16. *)
  gp0 gpu 0xE1000508;
  gp0 gpu 0xE2000041;
  gp0 gpu 0x65FFFFFF;
  gp0 gpu 0x00640064;
  gp0 gpu 0x00000000;
  gp0 gpu 0x00200010;

  (* Offset bits replace the masked coordinate bits. *)
  gp0 gpu 0xE2000401;
  gp0 gpu 0x65FFFFFF;
  gp0 gpu 0x008C008C;
  gp0 gpu 0x00000000;
  gp0 gpu 0x00010001;

  (* The hardware bypasses E2 for fixed 1x1 and 8x8 sprites, but applies it
     to fixed 16x16 sprites. *)
  gp0 gpu 0x75FFFFFF;
  gp0 gpu 0x00A000A0;
  gp0 gpu 0x00000000;
  gp0 gpu 0x6DFFFFFF;
  gp0 gpu 0x00A000B4;
  gp0 gpu 0x00000000;
  gp0 gpu 0x7DFFFFFF;
  gp0 gpu 0x00A000C8;
  gp0 gpu 0x00000000;

  (* Exercise the other texture-page X/Y bases used by the demo. *)
  st.Renderer.vram.(768) <- 0x9234;
  st.Renderer.vram.((256 * Renderer.vram_width) + 512) <- 0xA345;
  gp0 gpu 0xE2000000;
  gp0 gpu 0xE100050C;
  gp0 gpu 0x65FFFFFF;
  gp0 gpu 0x00B400DC;
  gp0 gpu 0x00000000;
  gp0 gpu 0x00010001;
  gp0 gpu 0xE1000518;
  gp0 gpu 0x65FFFFFF;
  gp0 gpu 0x00B400DD;
  gp0 gpu 0x00000000;
  gp0 gpu 0x00010001;
  drain_renderer ();

  let source x y = 0x8000 lor (x + 1) lor ((y + 1) lsl 5) in
  assert_pixel st 100 100 (source 0 0);
  assert_pixel st 108 100 (source 0 0);
  assert_pixel st 100 116 (source 0 0);
  assert_pixel st 108 116 (source 0 0);
  assert_pixel st 140 140 (source 8 0);
  assert_pixel st 160 160 (source 0 0);
  assert_pixel st 180 160 (source 0 0);
  assert_pixel st 200 160 (source 8 0);
  assert_pixel st 220 180 0x9234;
  assert_pixel st 221 180 0xA345

let install_clut4_texture st =
  (* Page X=512: texels 0..7 use palette entry 1 and texels 8..15 use
     palette entry 3. Each VRAM halfword contains four 4BPP texels. *)
  for y = 0 to 31 do
    st.Renderer.vram.((y * Renderer.vram_width) + 512) <- 0x1111;
    st.Renderer.vram.((y * Renderer.vram_width) + 513) <- 0x1111;
    st.Renderer.vram.((y * Renderer.vram_width) + 514) <- 0x3333;
    st.Renderer.vram.((y * Renderer.vram_width) + 515) <- 0x3333
  done;
  let clut_base = (256 * Renderer.vram_width) + 512 in
  st.Renderer.vram.(clut_base) <- 0x0000;
  st.Renderer.vram.(clut_base + 1) <- 0xBDEF;
  st.Renderer.vram.(clut_base + 2) <- 0x8000;
  st.Renderer.vram.(clut_base + 3) <- 0x801F

let test_clut4_rectangles_and_window () =
  let st = !Renderer.current in
  Renderer.reset_state st;
  install_clut4_texture st;
  let gpu = Gpu.create () in

  (* Directly verify nibble order and CLUT coordinate decoding. *)
  if Renderer.sample_texture st 0x0008 0x4020 0 0 <> 0xBDEF then
    failwith "4BPP CLUT lookup for low nibble failed";
  if Renderer.sample_texture st 0x0008 0x4020 8 0 <> 0x801F then
    failwith "4BPP CLUT lookup across packed halfwords failed";

  (* A mask of one clears U bit 3, repeating the first eight texels. *)
  gp0 gpu 0xE1000408;
  gp0 gpu 0xE2000001;
  gp0 gpu 0x65FFFFFF;
  gp0 gpu 0x00320032;
  gp0 gpu 0x40200000;
  gp0 gpu 0x00010010;

  (* Fixed-size sprites carry the same CLUT attribute. *)
  gp0 gpu 0xE2000000;
  gp0 gpu 0x6DFFFFFF;
  gp0 gpu 0x003C0046;
  gp0 gpu 0x40200008;

  (* Palette bit 15 controls semi-transparency for indexed textures. *)
  gp0 gpu 0x67FFFFFF;
  gp0 gpu 0x003C0050;
  gp0 gpu 0x40200000;
  gp0 gpu 0x00010001;
  drain_renderer ();
  assert_pixel st 50 50 0xBDEF;
  assert_pixel st 58 50 0xBDEF;
  assert_pixel st 70 60 0x801F;
  assert_pixel st 80 60 0x9CE7

let send_clut4_triangle gpu opcode color texpage =
  gp0 gpu ((opcode lsl 24) lor color);
  gp0 gpu 0x00140014;
  gp0 gpu 0x40200000;
  gp0 gpu 0x00140024;
  gp0 gpu (texpage lsl 16);
  gp0 gpu 0x00240014;
  gp0 gpu 0x00000000

let test_clut4_polygons_clip_and_dither () =
  let st = !Renderer.current in
  Renderer.reset_state st;
  install_clut4_texture st;
  let gpu = Gpu.create () in

  (* Limit rasterization to 24..30 in both axes, then draw a modulated 4BPP
     triangle with dithering enabled through E1 bit 9. *)
  gp0 gpu 0xE3006018;
  gp0 gpu 0xE400781E;
  gp0 gpu 0xE1000600;
  send_clut4_triangle gpu 0x24 0x808080 0x0008;
  drain_renderer ();
  assert_pixel st 23 24 0x0000;
  assert_pixel st 24 24 0xB9CE;
  assert_pixel st 25 24 0xBDEF;
  assert_pixel st 31 24 0x0000;

  (* Raw polygons return the exact CLUT color, including its high bit. *)
  Renderer.reset_state st;
  install_clut4_texture st;
  let gpu = Gpu.create () in
  gp0 gpu 0xE1000400;
  send_clut4_triangle gpu 0x25 0xFFFFFF 0x0008;
  drain_renderer ();
  assert_pixel st 24 24 0xBDEF;
  if gpu.Gpu.draw_mode land 0x9FF <> 0x0008 then
    failwith "textured polygon did not update the GPU texpage state"

let install_clut8_texture st =
  (* In 8BPP mode each halfword contains the even-U palette index in its low
     byte and the odd-U index in its high byte. *)
  for y = 0 to 63 do
    for x = 0 to 3 do
      st.Renderer.vram.((y * Renderer.vram_width) + 512 + x) <- 0x0101
    done;
    for x = 4 to 7 do
      st.Renderer.vram.((y * Renderer.vram_width) + 512 + x) <- 0x0303
    done
  done;
  (* Keep a distinct even/odd pair for byte-order validation. *)
  st.Renderer.vram.(512) <- 0x0301;
  let clut_base = (256 * Renderer.vram_width) + 512 in
  st.Renderer.vram.(clut_base) <- 0x0000;
  st.Renderer.vram.(clut_base + 1) <- 0xBDEF;
  st.Renderer.vram.(clut_base + 2) <- 0x8000;
  st.Renderer.vram.(clut_base + 3) <- 0x801F;
  st.Renderer.vram.(clut_base + 255) <- 0x83E0

let test_clut8_rectangles_and_window () =
  let st = !Renderer.current in
  Renderer.reset_state st;
  install_clut8_texture st;
  let gpu = Gpu.create () in

  if Renderer.sample_texture st 0x0088 0x4020 0 0 <> 0xBDEF then
    failwith "8BPP low-byte palette index failed";
  if Renderer.sample_texture st 0x0088 0x4020 1 0 <> 0x801F then
    failwith "8BPP high-byte palette index failed";
  st.Renderer.vram.(512) <- 0xFF01;
  if Renderer.sample_texture st 0x0088 0x4020 1 0 <> 0x83E0 then
    failwith "8BPP 256-entry CLUT addressing failed";
  st.Renderer.vram.(512) <- 0x0101;

  gp0 gpu 0xE1000488;
  gp0 gpu 0xE2000001;
  gp0 gpu 0x65FFFFFF;
  gp0 gpu 0x00320032;
  gp0 gpu 0x40200000;
  gp0 gpu 0x00010010;

  gp0 gpu 0xE2000000;
  gp0 gpu 0x6DFFFFFF;
  gp0 gpu 0x003C0046;
  gp0 gpu 0x40200008;
  gp0 gpu 0x67FFFFFF;
  gp0 gpu 0x003C0050;
  gp0 gpu 0x40200000;
  gp0 gpu 0x00010001;
  drain_renderer ();
  assert_pixel st 50 50 0xBDEF;
  assert_pixel st 58 50 0xBDEF;
  assert_pixel st 70 60 0x801F;
  assert_pixel st 80 60 0x9CE7

let send_clut8_triangle gpu opcode color texpage =
  gp0 gpu ((opcode lsl 24) lor color);
  gp0 gpu 0x00140014;
  gp0 gpu 0x40200000;
  gp0 gpu 0x00140024;
  gp0 gpu (texpage lsl 16);
  gp0 gpu 0x00240014;
  gp0 gpu 0x00000000

let send_clut8_quad gpu opcode color texpage =
  gp0 gpu ((opcode lsl 24) lor color);
  gp0 gpu 0x00280028;
  gp0 gpu 0x40200000;
  gp0 gpu 0x00280038;
  gp0 gpu (texpage lsl 16);
  gp0 gpu 0x00380028;
  gp0 gpu 0x00000000;
  gp0 gpu 0x00380038;
  gp0 gpu 0x00000000

let test_clut8_polygons_clip_dither_and_shading () =
  let st = !Renderer.current in
  Renderer.reset_state st;
  install_clut8_texture st;
  let gpu = Gpu.create () in
  gp0 gpu 0xE3006018;
  gp0 gpu 0xE400781E;
  gp0 gpu 0xE1000600;
  send_clut8_triangle gpu 0x24 0x808080 0x0088;
  drain_renderer ();
  assert_pixel st 23 24 0x0000;
  assert_pixel st 24 24 0xB9CE;
  assert_pixel st 25 24 0xBDEF;
  assert_pixel st 31 24 0x0000;

  Renderer.reset_state st;
  install_clut8_texture st;
  let gpu = Gpu.create () in
  gp0 gpu 0xE1000400;
  send_clut8_quad gpu 0x2D 0xFFFFFF 0x0088;

  (* Gouraud textured triangle: all three colors are neutral modulation. *)
  gp0 gpu 0x34808080;
  gp0 gpu 0x00460046;
  gp0 gpu 0x40200000;
  gp0 gpu 0x00808080;
  gp0 gpu 0x00460056;
  gp0 gpu 0x00880000;
  gp0 gpu 0x00808080;
  gp0 gpu 0x00560046;
  gp0 gpu 0x00000000;

  (* Gouraud textured quad exercises the longest indexed polygon packet. *)
  gp0 gpu 0x3C808080;
  gp0 gpu 0x00460064;
  gp0 gpu 0x40200000;
  gp0 gpu 0x00808080;
  gp0 gpu 0x00460074;
  gp0 gpu 0x00880000;
  gp0 gpu 0x00808080;
  gp0 gpu 0x00560064;
  gp0 gpu 0x00000000;
  gp0 gpu 0x00808080;
  gp0 gpu 0x00560074;
  gp0 gpu 0x00000000;
  drain_renderer ();
  assert_pixel st 44 44 0xBDEF;
  assert_pixel st 74 74 0xBDEF;
  assert_pixel st 104 74 0xBDEF;
  if gpu.Gpu.draw_mode land 0x9FF <> 0x0088 then
    failwith "8BPP polygon texpage state was not retained"

let send_raw_15bpp_triangle gpu x y =
  gp0 gpu 0x25FFFFFF;
  gp0 gpu (xy x y);
  gp0 gpu 0x00000000;
  gp0 gpu (xy x (y + 8));
  gp0 gpu 0x01080000;
  gp0 gpu (xy (x + 8) y);
  gp0 gpu 0x00000000

let test_mask_bit_setting () =
  let st = !Renderer.current in
  Renderer.reset_state st;
  let gpu = Gpu.create () in

  (* E6 bit 0 forces bit 15 on primitive output and is reported in
     GPUSTAT bit 11. *)
  gp0 gpu 0xE6000001;
  gp0 gpu 0x600000FF;
  gp0 gpu (xy 10 10);
  gp0 gpu 0x00010001;
  drain_renderer ();
  assert_pixel st 10 10 0x801F;
  if Gpu.gpustat gpu land (3 lsl 11) <> 1 lsl 11 then
    failwith "GPUSTAT did not report set-mask mode";

  (* E6 bit 1 protects destinations whose existing bit 15 is set. *)
  st.Renderer.vram.((20 * Renderer.vram_width) + 20) <- 0x801F;
  st.Renderer.vram.((20 * Renderer.vram_width) + 21) <- 0x001F;
  gp0 gpu 0xE6000002;
  gp0 gpu 0x6000FF00;
  gp0 gpu (xy 20 20);
  gp0 gpu 0x00010002;
  drain_renderer ();
  assert_pixel st 20 20 0x801F;
  assert_pixel st 21 20 0x03E0;
  if Gpu.gpustat gpu land (3 lsl 11) <> 2 lsl 11 then
    failwith "GPUSTAT did not report check-mask mode";

  (* With both controls set, the first draw masks the destination and the
     second draw is rejected. *)
  gp0 gpu 0xE6000003;
  gp0 gpu 0x600000FF;
  gp0 gpu (xy 30 30);
  gp0 gpu 0x00010001;
  gp0 gpu 0x6000FF00;
  gp0 gpu (xy 30 30);
  gp0 gpu 0x00010001;
  drain_renderer ();
  assert_pixel st 30 30 0x801F;

  (* CPU-to-VRAM and VRAM-to-VRAM transfers obey E6 too. *)
  gp0 gpu 0xE6000001;
  gp0 gpu 0xA0000000;
  gp0 gpu (xy 40 40);
  gp0 gpu 0x00010001;
  gp0 gpu 0x0000001F;
  drain_renderer ();
  assert_pixel st 40 40 0x801F;

  st.Renderer.vram.((50 * Renderer.vram_width) + 50) <- 0x801F;
  st.Renderer.vram.((50 * Renderer.vram_width) + 51) <- 0x0000;
  st.Renderer.vram.((50 * Renderer.vram_width) + 60) <- 0x03E0;
  st.Renderer.vram.((50 * Renderer.vram_width) + 61) <- 0x001F;
  gp0 gpu 0xE6000002;
  gp0 gpu 0x80000000;
  gp0 gpu (xy 60 50);
  gp0 gpu (xy 50 50);
  gp0 gpu 0x00010002;
  drain_renderer ();
  assert_pixel st 50 50 0x801F;
  assert_pixel st 51 50 0x001F;

  (* The raw texture bit is inherited normally, then E6 can force it. A
     protected polygon cannot subsequently be overwritten. *)
  st.Renderer.vram.(512) <- 0x001F;
  gp0 gpu 0xE6000003;
  send_raw_15bpp_triangle gpu 70 70;
  drain_renderer ();
  assert_pixel st 72 72 0x801F;
  st.Renderer.vram.(512) <- 0x03E0;
  gp0 gpu 0xE6000002;
  send_raw_15bpp_triangle gpu 70 70;
  drain_renderer ();
  assert_pixel st 72 72 0x801F;

  (* GP0(02h) Fill-VRAM is explicitly exempt from both mask controls. *)
  st.Renderer.vram.((90 * Renderer.vram_width) + 90) <- 0xFFFF;
  gp0 gpu 0xE6000003;
  gp0 gpu 0x020000FF;
  gp0 gpu (xy 90 90);
  gp0 gpu 0x00010001;
  drain_renderer ();
  assert_pixel st 90 90 0x001F

let () =
  test_textured_rectangles ();
  test_texture_windows ();
  test_clut4_rectangles_and_window ();
  test_clut4_polygons_clip_and_dither ();
  test_clut8_rectangles_and_window ();
  test_clut8_polygons_clip_dither_and_shading ();
  test_mask_bit_setting ()
