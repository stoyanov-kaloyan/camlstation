open Cpu
open Rom

external init_renderer : unit -> unit = "init_renderer"
external should_close : unit -> bool = "should_close"

let bios = open_rom "./SCPH1001.BIN"

let run_machine () =
  let cpu = cpu_of_bios bios in
  let rec loop () =
    if should_close () then ()
    else (
      step cpu;
      loop ())
  in
  init_renderer ();
  loop ()
