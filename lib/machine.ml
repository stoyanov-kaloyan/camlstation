open Cpu
open Rom

let bios = open_rom "./SCPH1001.BIN"

let run_machine () =
  let cpu = cpu_of_bios bios in
  let rec loop () =
    step cpu;
    loop ()
  in
  loop ()