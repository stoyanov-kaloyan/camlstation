let clamp value low high = max low (min high value)

let decode_adpcm_block (block : int array) (decoded : int array)
    (old : int ref) (older : int ref) : unit =
  if Array.length block <> 16 then
    invalid_arg "Spu.decode_adpcm_block: block must contain 16 bytes";
  if Array.length decoded <> 28 then
    invalid_arg "Spu.decode_adpcm_block: decoded must contain 28 samples";

  let header = block.(0) land 0xFF in
  let encoded_shift = header land 0x0F in
  let shift = if encoded_shift > 12 then 9 else encoded_shift in
  let filter = min 4 ((header lsr 4) land 0x07) in

  for sample_idx = 0 to 27 do
    let sample_byte = block.(2 + (sample_idx / 2)) land 0xFF in
    let sample_nibble =
      (sample_byte lsr (4 * (sample_idx mod 2))) land 0x0F
    in
    let raw_sample =
      if sample_nibble land 0x08 <> 0 then sample_nibble - 16
      else sample_nibble
    in
    let shifted_sample = raw_sample lsl (12 - shift) in
    let filtered_sample =
      match filter with
      | 0 -> shifted_sample
      | 1 -> shifted_sample + (((60 * !old) + 32) / 64)
      | 2 -> shifted_sample + (((115 * !old) - (52 * !older) + 32) / 64)
      | 3 -> shifted_sample + (((98 * !old) - (55 * !older) + 32) / 64)
      | 4 -> shifted_sample + (((122 * !old) - (60 * !older) + 32) / 64)
      | _ -> assert false
    in
    let clamped_sample = clamp filtered_sample (-0x8000) 0x7FFF in
    decoded.(sample_idx) <- clamped_sample;
    older := !old;
    old := clamped_sample
  done
