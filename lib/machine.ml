open Cpu
open Rom

let bios = open_rom "./roms/SCPH1001.BIN"

let run_machine () =
  let cpu = cpu_of_bios bios in
  let rec loop () =
    if Renderer.should_close () then ()
    else (
      step cpu;
      loop ())
  in
  Renderer.init ();
  loop ();
  Renderer.shutdown ()
