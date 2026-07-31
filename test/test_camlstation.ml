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

let () =
  test_textured_rectangles ();
  test_texture_windows ()
