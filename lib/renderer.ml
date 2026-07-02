type triangle_vertex = { x : int; y : int; color : int }
type quad_vertex = { qx : int; qy : int; qcolor : int }

type image_transfer_state = {
  mutable image_load_active : bool;
  mutable image_x : int;
  mutable image_y : int;
  mutable image_w : int;
  mutable image_h : int;
  mutable image_cur_x : int;
  mutable image_cur_y : int;
  mutable image_words_remaining : int;
}

type command =
  | Fill of int * int * int
  | Rect of bool * int * int * int
  | LineFlat of int * int * int * int
  | LineShaded of int * int * int * int * int * int * int
  | PolygonFlatTri of bool * int * int * int * int
  | PolygonShadedTri of bool * int * int * int * int * int * int
  | PolygonFlatQuad of bool * int * int * int * int * int
  | PolygonShadedQuad of bool * int * int * int * int * int * int * int * int
  | VramCopy of int * int * int
  | ImageBegin of int * int
  | ImageWord of int
  | DisplayReset
  | DrawAreaTopLeft of int
  | DrawAreaBottomRight of int
  | DrawMode of int
  | DisplayArea of int
  | DisplayHRange of int
  | DisplayVRange of int
  | DisplayMode of int

type state = {
  mutable close_requested : bool;
  mutable stop_requested : bool;
  vram : int array;
  upload_pixels : int array;
  queue : command Queue.t;
  queue_mutex : Mutex.t;
  image_state : image_transfer_state;
  mutable draw_area_left : int;
  mutable draw_area_top : int;
  mutable draw_area_right : int;
  mutable draw_area_bottom : int;
  mutable display_x : int;
  mutable display_y : int;
  mutable display_w : int;
  mutable display_h : int;
  mutable dither_enabled : bool;
  mutable render_thread : Thread.t option;
}

let vram_width = 1024
let vram_height = 512

let clamp value lower_bound upper_bound =
  max lower_bound (min upper_bound value)

external host_init : unit -> unit = "init_renderer"
external host_poll_events : unit -> unit = "renderer_poll_events"
external host_poll_close : unit -> bool = "should_close"

external host_present : int array -> int -> int -> int -> int -> unit
  = "renderer_present_frame"

let current =
  let pixel_count = vram_width * vram_height in
  ref
    {
      close_requested = false;
      stop_requested = false;
      vram = Array.make pixel_count 0;
      upload_pixels = Array.make pixel_count 0;
      queue = Queue.create ();
      queue_mutex = Mutex.create ();
      image_state =
        {
          image_load_active = false;
          image_x = 0;
          image_y = 0;
          image_w = 0;
          image_h = 0;
          image_cur_x = 0;
          image_cur_y = 0;
          image_words_remaining = 0;
        };
      draw_area_left = 0;
      draw_area_top = 0;
      draw_area_right = vram_width - 1;
      draw_area_bottom = vram_height - 1;
      display_x = 0;
      display_y = 0;
      display_w = 320;
      display_h = 240;
      dither_enabled = false;
      render_thread = None;
    }

let reset_state st =
  st.close_requested <- false;
  st.stop_requested <- false;
  Array.fill st.vram 0 (Array.length st.vram) 0;
  st.draw_area_left <- 0;
  st.draw_area_top <- 0;
  st.draw_area_right <- vram_width - 1;
  st.draw_area_bottom <- vram_height - 1;
  st.display_x <- 0;
  st.display_y <- 0;
  st.display_w <- 320;
  st.display_h <- 240;
  st.dither_enabled <- false;
  st.image_state.image_load_active <- false;
  st.image_state.image_x <- 0;
  st.image_state.image_y <- 0;
  st.image_state.image_w <- 0;
  st.image_state.image_h <- 0;
  st.image_state.image_cur_x <- 0;
  st.image_state.image_cur_y <- 0;
  st.image_state.image_words_remaining <- 0;
  Mutex.lock st.queue_mutex;
  Queue.clear st.queue;
  Mutex.unlock st.queue_mutex

let should_close () = !current.close_requested || host_poll_close ()

let shutdown () =
  let st = !current in
  st.stop_requested <- true;
  (match st.render_thread with None -> () | Some thread -> Thread.join thread);
  st.render_thread <- None

let queue_command cmd =
  let st = !current in
  Mutex.lock st.queue_mutex;
  Queue.push cmd st.queue;
  Mutex.unlock st.queue_mutex

let rgb24_to_rgb555 rgb =
  let r8 = rgb land 0xFF in
  let g8 = (rgb lsr 8) land 0xFF in
  let b8 = (rgb lsr 16) land 0xFF in
  let r5 = ((r8 * 31) + 127) / 255 in
  let g5 = ((g8 * 31) + 127) / 255 in
  let b5 = ((b8 * 31) + 127) / 255 in
  r5 lor (g5 lsl 5) lor (b5 lsl 10)

let five_to_eight x = ((x * 255) + 15) / 31

let rgb555_to_argb32 p =
  let r = five_to_eight (p land 0x1F) in
  let g = five_to_eight ((p lsr 5) land 0x1F) in
  let b = five_to_eight ((p lsr 10) land 0x1F) in
  0xFF000000 lor (r lsl 16) lor (g lsl 8) lor b

let blend_rgb555 src dst =
  let sr = src land 0x1F in
  let sg = (src lsr 5) land 0x1F in
  let sb = (src lsr 10) land 0x1F in
  let dr = dst land 0x1F in
  let dg = (dst lsr 5) land 0x1F in
  let db = (dst lsr 10) land 0x1F in
  (sr + dr) / 2 lor (((sg + dg) / 2) lsl 5) lor (((sb + db) / 2) lsl 10)

let dither_offset x y =
  match (y land 3, x land 3) with
  | 0, 0 -> -4
  | 0, 1 -> 0
  | 0, 2 -> -3
  | 0, 3 -> 1
  | 1, 0 -> 2
  | 1, 1 -> -2
  | 1, 2 -> 3
  | 1, 3 -> -1
  | 2, 0 -> -3
  | 2, 1 -> 1
  | 2, 2 -> -4
  | 2, 3 -> 0
  | _ -> if x land 1 = 0 then 3 else -1

let in_draw_area x y st =
  x >= st.draw_area_left && x <= st.draw_area_right && y >= st.draw_area_top
  && y <= st.draw_area_bottom

let write_vram_pixel st x y value =
  if x >= 0 && x < vram_width && y >= 0 && y < vram_height then
    st.vram.((y * vram_width) + x) <- value

let begin_image_load st arg0 arg1 =
  let img = st.image_state in
  img.image_x <- arg0 land 0x3FF;
  img.image_y <- (arg0 lsr 16) land 0x1FF;
  img.image_w <- arg1 land 0xFFFF;
  img.image_h <- (arg1 lsr 16) land 0xFFFF;
  if img.image_w <= 0 || img.image_h <= 0 then (
    img.image_load_active <- false;
    img.image_words_remaining <- 0)
  else (
    img.image_words_remaining <- ((img.image_w * img.image_h) + 1) / 2;
    img.image_cur_x <- 0;
    img.image_cur_y <- 0;
    img.image_load_active <- true)

let advance_image_cursor img =
  img.image_cur_x <- img.image_cur_x + 1;
  if img.image_cur_x >= img.image_w then (
    img.image_cur_x <- 0;
    img.image_cur_y <- img.image_cur_y + 1)

let consume_image_word st word =
  let img = st.image_state in
  if img.image_load_active then (
    let px0 = word land 0xFFFF in
    let px1 = (word lsr 16) land 0xFFFF in
    write_vram_pixel st
      (img.image_x + img.image_cur_x)
      (img.image_y + img.image_cur_y)
      px0;
    advance_image_cursor img;
    if img.image_cur_y < img.image_h then (
      write_vram_pixel st
        (img.image_x + img.image_cur_x)
        (img.image_y + img.image_cur_y)
        px1;
      advance_image_cursor img);
    img.image_words_remaining <- img.image_words_remaining - 1;
    if img.image_words_remaining <= 0 || img.image_cur_y >= img.image_h then (
      img.image_words_remaining <- 0;
      img.image_load_active <- false))

let draw_line_flat st x0 y0 x1 y1 color =
  let dx = abs (x1 - x0) in
  let dy = -abs (y1 - y0) in
  let sx = if x0 < x1 then 1 else -1 in
  let sy = if y0 < y1 then 1 else -1 in
  let err = ref (dx + dy) in
  let x = ref x0 in
  let y = ref y0 in
  let rec loop () =
    write_vram_pixel st !x !y color;
    if !x <> x1 || !y <> y1 then (
      let e2 = 2 * !err in
      if e2 >= dy then (
        err := !err + dy;
        x := !x + sx);
      if e2 <= dx then (
        err := !err + dx;
        y := !y + sy);
      loop ())
  in
  loop ()

let draw_line_shaded st x0 y0 c0 x1 y1 c1 =
  let steps = max (abs (x1 - x0)) (abs (y1 - y0)) in
  if steps = 0 then write_vram_pixel st x0 y0 c0
  else
    let r0 = c0 land 0x1F
    and g0 = (c0 lsr 5) land 0x1F
    and b0 = (c0 lsr 10) land 0x1F in
    let r1 = c1 land 0x1F
    and g1 = (c1 lsr 5) land 0x1F
    and b1 = (c1 lsr 10) land 0x1F in
    for i = 0 to steps do
      let t = float i /. float steps in
      let x = int_of_float (Float.round (float x0 +. (float (x1 - x0) *. t))) in
      let y = int_of_float (Float.round (float y0 +. (float (y1 - y0) *. t))) in
      let r = int_of_float (Float.round (float r0 +. (float (r1 - r0) *. t))) in
      let g = int_of_float (Float.round (float g0 +. (float (g1 - g0) *. t))) in
      let b = int_of_float (Float.round (float b0 +. (float (b1 - b0) *. t))) in
      write_vram_pixel st x y
        (r land 0x1F lor ((g land 0x1F) lsl 5) lor ((b land 0x1F) lsl 10))
    done

let is_top_left_edge a b = a.y < b.y || (a.y = b.y && a.x > b.x)

let draw_filled_triangle st v0 v1 v2 semi_transparent =
  let a = ref v0 and b = ref v1 and c = ref v2 in
  let area =
    ((!b.x - !a.x) * (!c.y - !a.y)) - ((!b.y - !a.y) * (!c.x - !a.x))
  in
  if area <> 0 then (
    if area < 0 then (
      let tmp = !b in
      b := !c;
      c := tmp);
    let min_x = max 0 (max st.draw_area_left (min !a.x (min !b.x !c.x))) in
    let max_x =
      min (vram_width - 1) (min st.draw_area_right (max !a.x (max !b.x !c.x)))
    in
    let min_y = max 0 (max st.draw_area_top (min !a.y (min !b.y !c.y))) in
    let max_y =
      min (vram_height - 1) (min st.draw_area_bottom (max !a.y (max !b.y !c.y)))
    in
    let inv_area = 1.0 /. float area in
    let r0 = !a.color land 0x1F
    and g0 = (!a.color lsr 5) land 0x1F
    and b0 = (!a.color lsr 10) land 0x1F in
    let r1 = !b.color land 0x1F
    and g1 = (!b.color lsr 5) land 0x1F
    and b1 = (!b.color lsr 10) land 0x1F in
    let r2 = !c.color land 0x1F
    and g2 = (!c.color lsr 5) land 0x1F
    and b2 = (!c.color lsr 10) land 0x1F in
    let edge p0 p1 px py =
      ((p1.x - p0.x) * (py - p0.y)) - ((p1.y - p0.y) * (px - p0.x))
    in
    for y = min_y to max_y do
      for x = min_x to max_x do
        let w0 = edge !b !c x y in
        let w1 = edge !c !a x y in
        let w2 = edge !a !b x y in
        let inside =
          (w0 > 0 || (w0 = 0 && is_top_left_edge !b !c))
          && (w1 > 0 || (w1 = 0 && is_top_left_edge !c !a))
          && (w2 > 0 || (w2 = 0 && is_top_left_edge !a !b))
        in
        if inside then
          let wa = float w0 *. inv_area in
          let wb = float w1 *. inv_area in
          let wc = float w2 *. inv_area in
          let r =
            int_of_float
              (Float.round
                 ((float r0 *. wa) +. (float r1 *. wb) +. (float r2 *. wc)))
          in
          let g =
            int_of_float
              (Float.round
                 ((float g0 *. wa) +. (float g1 *. wb) +. (float g2 *. wc)))
          in
          let bl =
            int_of_float
              (Float.round
                 ((float b0 *. wa) +. (float b1 *. wb) +. (float b2 *. wc)))
          in
          let r =
            if st.dither_enabled then clamp (r + dither_offset x y) 0 31
            else clamp r 0 31
          in
          let g =
            if st.dither_enabled then clamp (g + dither_offset x y) 0 31
            else clamp g 0 31
          in
          let bl =
            if st.dither_enabled then clamp (bl + dither_offset x y) 0 31
            else clamp bl 0 31
          in
          let color = r lor (g lsl 5) lor (bl lsl 10) in
          let color =
            if semi_transparent then
              blend_rgb555 color st.vram.((y * vram_width) + x)
            else color
          in
          st.vram.((y * vram_width) + x) <- color
      done
    done)

let draw_filled_quad st v0 v1 v2 v3 semi_transparent =
  draw_filled_triangle st
    { x = v0.qx; y = v0.qy; color = v0.qcolor }
    { x = v1.qx; y = v1.qy; color = v1.qcolor }
    { x = v2.qx; y = v2.qy; color = v2.qcolor }
    semi_transparent;
  draw_filled_triangle st
    { x = v1.qx; y = v1.qy; color = v1.qcolor }
    { x = v2.qx; y = v2.qy; color = v2.qcolor }
    { x = v3.qx; y = v3.qy; color = v3.qcolor }
    semi_transparent

let fill_rect st x y w h color =
  if w > 0 && h > 0 then
    let x0 = max 0 (max x st.draw_area_left) in
    let y0 = max 0 (max y st.draw_area_top) in
    let x1 = min (vram_width - 1) (min (x + w - 1) st.draw_area_right) in
    let y1 = min (vram_height - 1) (min (y + h - 1) st.draw_area_bottom) in
    for py = y0 to y1 do
      let row = py * vram_width in
      for px = x0 to x1 do
        st.vram.(row + px) <- color
      done
    done

let process_command st = function
  | Fill (rgb, xy, wh) ->
      let color = rgb24_to_rgb555 (rgb land 0x00FFFFFF) in
      let x = xy land 0x3FF in
      let y = (xy lsr 16) land 0x1FF in
      let w = if wh land 0x3FF = 0 then vram_width else wh land 0x3FF in
      let h =
        if (wh lsr 16) land 0x1FF = 0 then vram_height
        else (wh lsr 16) land 0x1FF
      in
      fill_rect st x y w h color
  | Rect (semi, rgb, xy, wh) ->
      let color = rgb24_to_rgb555 (rgb land 0x00FFFFFF) in
      let x = xy land 0x3FF in
      let y = (xy lsr 16) land 0x1FF in
      let w = wh land 0xFFFF in
      let h = (wh lsr 16) land 0xFFFF in
      if semi then
        let x0 = max 0 (max x st.draw_area_left) in
        let y0 = max 0 (max y st.draw_area_top) in
        let x1 = min (vram_width - 1) (min (x + w - 1) st.draw_area_right) in
        let y1 = min (vram_height - 1) (min (y + h - 1) st.draw_area_bottom) in
        for py = y0 to y1 do
          let row = py * vram_width in
          for px = x0 to x1 do
            let idx = row + px in
            st.vram.(idx) <- blend_rgb555 color st.vram.(idx)
          done
        done
      else fill_rect st x y w h color
  | LineFlat (color, xy0, xy1, _) ->
      draw_line_flat st (xy0 land 0x3FF)
        ((xy0 lsr 16) land 0x1FF)
        (xy1 land 0x3FF)
        ((xy1 lsr 16) land 0x1FF)
        (color land 0x7FFF)
  | LineShaded (c0, xy0, c1, xy1, _, _, _) ->
      draw_line_shaded st (xy0 land 0x3FF)
        ((xy0 lsr 16) land 0x1FF)
        (c0 land 0x7FFF) (xy1 land 0x3FF)
        ((xy1 lsr 16) land 0x1FF)
        (c1 land 0x7FFF)
  | PolygonFlatTri (semi, color, xy0, xy1, xy2) ->
      let color = color land 0x7FFF in
      draw_filled_triangle st
        { x = xy0 land 0x3FF; y = (xy0 lsr 16) land 0x1FF; color }
        { x = xy1 land 0x3FF; y = (xy1 lsr 16) land 0x1FF; color }
        { x = xy2 land 0x3FF; y = (xy2 lsr 16) land 0x1FF; color }
        semi
  | PolygonShadedTri (semi, c0, xy0, c1, xy1, c2, xy2) ->
      draw_filled_triangle st
        {
          x = xy0 land 0x3FF;
          y = (xy0 lsr 16) land 0x1FF;
          color = c0 land 0x7FFF;
        }
        {
          x = xy1 land 0x3FF;
          y = (xy1 lsr 16) land 0x1FF;
          color = c1 land 0x7FFF;
        }
        {
          x = xy2 land 0x3FF;
          y = (xy2 lsr 16) land 0x1FF;
          color = c2 land 0x7FFF;
        }
        semi
  | PolygonFlatQuad (semi, color, xy0, xy1, xy2, xy3) ->
      let color = color land 0x7FFF in
      draw_filled_quad st
        { qx = xy0 land 0x3FF; qy = (xy0 lsr 16) land 0x1FF; qcolor = color }
        { qx = xy1 land 0x3FF; qy = (xy1 lsr 16) land 0x1FF; qcolor = color }
        { qx = xy2 land 0x3FF; qy = (xy2 lsr 16) land 0x1FF; qcolor = color }
        { qx = xy3 land 0x3FF; qy = (xy3 lsr 16) land 0x1FF; qcolor = color }
        semi
  | PolygonShadedQuad (semi, c0, xy0, c1, xy1, c2, xy2, c3, xy3) ->
      draw_filled_quad st
        {
          qx = xy0 land 0x3FF;
          qy = (xy0 lsr 16) land 0x1FF;
          qcolor = c0 land 0x7FFF;
        }
        {
          qx = xy1 land 0x3FF;
          qy = (xy1 lsr 16) land 0x1FF;
          qcolor = c1 land 0x7FFF;
        }
        {
          qx = xy2 land 0x3FF;
          qy = (xy2 lsr 16) land 0x1FF;
          qcolor = c2 land 0x7FFF;
        }
        {
          qx = xy3 land 0x3FF;
          qy = (xy3 lsr 16) land 0x1FF;
          qcolor = c3 land 0x7FFF;
        }
        semi
  | VramCopy (src_xy, dst_xy, wh) ->
      let src_x = src_xy land 0x3FF in
      let src_y = (src_xy lsr 16) land 0x1FF in
      let dst_x = dst_xy land 0x3FF in
      let dst_y = (dst_xy lsr 16) land 0x1FF in
      let w = if wh land 0xFFFF = 0 then vram_width else wh land 0xFFFF in
      let h =
        if (wh lsr 16) land 0xFFFF = 0 then vram_height
        else (wh lsr 16) land 0xFFFF
      in
      let temp =
        Array.init (w * h) (fun i ->
            let x = i mod w in
            let y = i / w in
            let sx = src_x + x in
            let sy = src_y + y in
            if sx >= 0 && sx < vram_width && sy >= 0 && sy < vram_height then
              st.vram.((sy * vram_width) + sx)
            else 0)
      in
      for y = 0 to h - 1 do
        for x = 0 to w - 1 do
          let tx = dst_x + x in
          let ty = dst_y + y in
          if tx >= 0 && tx < vram_width && ty >= 0 && ty < vram_height then
            st.vram.((ty * vram_width) + tx) <- temp.((y * w) + x)
        done
      done
  | ImageBegin (arg0, arg1) -> begin_image_load st arg0 arg1
  | ImageWord word -> consume_image_word st word
  | DisplayReset ->
      st.display_x <- 0;
      st.display_y <- 0;
      st.display_w <- 320;
      st.display_h <- 240
  | DrawAreaTopLeft packed ->
      st.draw_area_left <- packed land 0x3FF;
      st.draw_area_top <- (packed lsr 10) land 0x3FF
  | DrawAreaBottomRight packed ->
      st.draw_area_right <- packed land 0x3FF;
      st.draw_area_bottom <- (packed lsr 10) land 0x3FF
  | DrawMode word -> st.dither_enabled <- word land (1 lsl 9) <> 0
  | DisplayArea packed ->
      st.display_x <- packed land 0x3FF;
      st.display_y <- (packed lsr 10) land 0x1FF
  | DisplayHRange packed ->
      let start = packed land 0xFFF in
      let finish = (packed lsr 12) land 0xFFF in
      let width = (finish - start) / 8 in
      if width > 0 then st.display_w <- width
  | DisplayVRange packed ->
      let start = packed land 0x3FF in
      let finish = (packed lsr 10) land 0x3FF in
      let height = finish - start in
      if height > 0 then st.display_h <- height
  | DisplayMode word ->
      let hres_lo = word land 0x3 in
      let hres_hi = (word lsr 6) land 0x1 <> 0 in
      st.display_w <-
        (if hres_hi then 368
         else if hres_lo = 0 then 256
         else if hres_lo = 1 then 320
         else if hres_lo = 2 then 512
         else 640);
      st.display_h <- (if (word lsr 2) land 0x1 <> 0 then 480 else 240)

let pump_once st =
  host_poll_events ();
  if host_poll_close () then st.close_requested <- true;
  let pending = Queue.create () in
  Mutex.lock st.queue_mutex;
  Queue.transfer st.queue pending;
  Mutex.unlock st.queue_mutex;
  while not (Queue.is_empty pending) do
    process_command st (Queue.pop pending)
  done;
  let len = Array.length st.vram in
  for i = 0 to len - 1 do
    st.upload_pixels.(i) <- rgb555_to_argb32 st.vram.(i)
  done;
  host_present st.upload_pixels st.display_x st.display_y st.display_w
    st.display_h

let rec render_loop st =
  if (not st.stop_requested) && not st.close_requested then (
    pump_once st;
    Thread.delay 0.016;
    render_loop st)

let init () =
  host_init ();
  let st = !current in
  reset_state st;
  if Option.is_none st.render_thread then
    st.render_thread <- Some (Thread.create (fun () -> render_loop st) ())

let submit cmd = queue_command cmd
