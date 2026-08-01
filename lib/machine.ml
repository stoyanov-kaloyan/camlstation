open Cpu
open Rom

let bios = open_rom "./roms/SCPH1001.BIN"

let run_machine () =
  let cpu = cpu_of_bios bios in
  let host_start = Unix.gettimeofday () in
  let emulated_start = cpu.cycle_count in
  let pace () =
    let elapsed_cycles = Int64.sub cpu.cycle_count emulated_start in
    let target =
      host_start
      +. (Int64.to_float elapsed_cycles /. float_of_int cpu_clock_hz)
    in
    let delay = target -. Unix.gettimeofday () in
    if delay > 0. then Thread.delay (min delay 0.01)
  in
  let instructions = ref 0 in
  let rec loop () =
    if Renderer.should_close () then ()
    else (
      step cpu;
      incr instructions;
      if !instructions land 0xFFF = 0 then pace ();
      loop ())
  in
  Renderer.init ();
  (* CPU runs in it's own domain so it is as decoupled as possible from presentation *)
  let cpu_domain = Domain.spawn loop in
  Renderer.run ();
  Domain.join cpu_domain;
  Renderer.shutdown ()
