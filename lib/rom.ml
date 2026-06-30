let open_rom filename =
  let ic = open_in_bin filename in
  let rom_size = in_channel_length ic in
  let rom_data = Bytes.create rom_size in
  really_input ic rom_data 0 rom_size;
  close_in ic;

  let gb4 = 4 * 1024 * 1024 in
  let bus : int array = Array.make gb4 0 in

  for i = 0 to rom_size - 1 do
    bus.(i) <- Char.code (Bytes.get rom_data i)
  done;

  bus

let print_rom rom_data len =
  for i = 0 to len - 1 do
    Printf.printf "%02X " (Char.code (Bytes.get rom_data i));
    if (i + 1) mod 16 = 0 then Printf.printf "\n"
  done;
  Printf.printf "\n"
