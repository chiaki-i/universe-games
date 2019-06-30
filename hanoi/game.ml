open UniverseJs
open Color
open Image
open World

(***** 型定義 ******)
type bar_t = {
  height     : float;             (* 軸に通されている輪の数 *)
  disk_sizes : float list;        (* 軸に通されている輪のサイズ *)
}

type disk_t = {
  left_top : float * float;     (* 左上の座標 *)
  prev     : (float * float) option;
                                (* 一つ前の座標 *)
  size     : float;             (* 輪の大きさ1,2,3 *)
  bar      : int;               (* 軸 左から1,2,3 *)
  pos      : float;             (* 軸のどの高さにあるか *)
  moved    : int;               (* この輪を動かした回数 *)
  color    : Color.t;
}

type mode_t = Disk3 | Disk5

type world_t = {
  disks : disk_t list;          (* 輪 *)
  move  : int;                  (* 動かすことができる既定の回数 *)
  remaining : int;              (* 残りの回数 *)
  mode  : mode_t;               (* 輪の枚数 *)
  msg   : string;               (* debugging *)
}

(***** 定数 *****)

let width  = 600.
let height = 400.
let disk_module = 30.
let bar1 = 100.
let bar2 = 300.
let bar3 = 500.
let bar_bot = 350.

(* 軸の定義 *)
let bars = [
  (rectangle 160. 20. sandyBrown); (rectangle 20. 200. sandyBrown);
  (rectangle 160. 20. sandyBrown); (rectangle 20. 200. sandyBrown);
  (rectangle 160. 20. sandyBrown); (rectangle 20. 200. sandyBrown);
]

(* 軸の位置 *)
let bars_posn = [
  (20., 350.);  (90., 150.);
  (220., 350.); (290., 150.);
  (420., 350.); (490., 150.);
]

(* 軸を含めた背景 *)
let background =
  place_images bars bars_posn (empty_scene width height)

let make_left_top (bar : int) (pos : float) (size : float) : (float * float) =
  let current_bar =
    if bar = 1 then bar1
    else if bar = 2 then bar2
    else bar3
  in
  (current_bar -. ((disk_module *. size) /. 2.),
   bar_bot -. (disk_module *. pos))
  
let make_disk size bar pos moved color : disk_t = {
  left_top = make_left_top bar pos size;
  prev = None;
  size = size;
  bar = bar;
  pos = pos;
  moved = moved;
  color = color;
}

let disk_image (disk : disk_t) : Image.t = match disk with
    {size = s; color = c} ->
    rectangle (disk_module *. s) disk_module c

let disk_left_top (disks : disk_t list) : (float * float) list =
  List.map (function {left_top = tl} -> tl) disks

let draw world = match world with {disks = d; remaining = m; msg = msg} ->
  let with_disks = 
    place_images (List.map disk_image d) (disk_left_top d) background in
  let with_moves =
    place_image
      (text (string_of_int m) gray10)
      (width -. 40., 0.)
      with_disks in
  let with_reset_button =
    place_image
      (text "reset" gray10)
      (7., 4.)
      (place_image
         (rectangle 55. 25. gray80)
         (5., 5.)      
         with_moves) in
  let with_disk3_button =
    place_image
      (text "3" gray10)
      (71., 5.)
      (place_image
         (rectangle 20. 25. gray80)
         (68., 5.)      
         with_reset_button) in
  let with_disk5_button =
    place_image
      (text "5" gray10)
      (98., 5.)
      (place_image
         (rectangle 20. 25. gray80)
         (95., 5.)      
         with_disk3_button) in
  let with_msg =
    place_image
      (text msg gray10)
      (width /. 2., 0.)
      with_disk5_button in
  with_msg

let initial_world_3 = {
  disks = [
    make_disk 1. 1 3. 0 gray20;
    make_disk 2. 1 2. 0 gray50;
    make_disk 3. 1 1. 0 gray80;
  ];
  move = 10;
  remaining = 10;
  mode = Disk3;
  msg = "";
}

let initial_world_5 = {
  disks = [
    make_disk 1. 1 5. 0 gray10;
    make_disk 2. 1 4. 0 gray30;
    make_disk 3. 1 3. 0 gray50;
    make_disk 4. 1 2. 0 gray70;
    make_disk 5. 1 1. 0 gray90;
  ];
  move = 35;
  remaining = 35;
  mode = Disk5;
  msg = "";
}

let fetch_bar_info (world : world_t) (num : int) : bar_t =
  match world with {disks = ds} ->
    let current_sizes =
      List.map
        (function Some x -> x | None -> 0.)
        (List.filter
           (function Some x -> true | None -> false)
           (List.map (fun {bar = b; size = s} -> if b = num then Some s else None) ds))
    in
    let current_height = List.length current_sizes in
    {height = float_of_int current_height; disk_sizes = current_sizes}

(***** inside? valid? *****)
let on_which_bar (x : float) (y : float) : int =
  if (50. <= x) && (x <= 150.) && (150. <= y) && (y <= 370.) then 1
  else if (250. <= x) && (x <= 350.) && (150. <= y) && (y <= 370.) then 2
  else if (450. <= x) && (x <= 550.) && (150. <= y) && (y <= 370.) then 3
  else 0

let on_reset_button (x : float) (y : float) : bool =
  if (5. <= x) && (x <= 60.) && (5. <= y) && (y <= 30.)
  then true
  else false

let on_disk3_button (x : float) (y : float) : bool =
  if (68. <= x) && (x <= 88.) && (5. <= y) && (y <= 30.)
  then true
  else false

let on_disk5_button (x : float) (y : float) : bool =
  if (95. <= x) && (x <= 105.) && (5. <= y) && (y <= 30.)
  then true
  else false

(***** マウス処理 *****)
let end_with (world : world_t) : world_t =
  match world with {disks = ds; move = m} ->
  match fetch_bar_info world 3 with {height = h3} ->
  match fetch_bar_info world 2 with {height = h2} ->
    let total_moved =
      List.fold_left (+) 0 (List.map (fun {moved = mvd} -> mvd) ds) in
    if (h3 = float_of_int (List.length ds)) || (* 一番右の軸に完成させた、もしくは *)
       (h2 = float_of_int (List.length ds))    (* 真ん中の軸に完成させた *)
    then {world with remaining = m - total_moved; msg = "; )"}
    else
      begin
        if total_moved = m                 (* 既定回数に到達 *)
        then {world with remaining = m - total_moved;
                         msg = "try again"}
        else if total_moved > m then world (* 既定回数以上は操作回数が減らない *)
        else {world with remaining = m - total_moved}
      end
         
let block_button_down (world : world_t) (x : float) (y : float) (d : disk_t) : disk_t =
  let current_bar = on_which_bar x y in
  match world with {remaining = remaining} ->
    if remaining = 0 then d
    else
      match fetch_bar_info world current_bar with
      | {disk_sizes = sizes} ->
        let min_size = List.fold_left (min) max_float sizes in
        match d with {left_top = (left, top); size = s} ->
          let right = left +. (s *. disk_module) in
          let bottom = top +. disk_module in
          if (left <= x) && (x <= right) && (top <= y) && (y <= bottom) && (s <= min_size)
          then        (* ブロックの中心座標 = マウスの座標 かつ 一番小さい（一番上の）ブロック *)
            let new_left = x -. (s *. disk_module /. 2.) in
            let new_top = y -. (disk_module /. 2.) in
            {d with prev = Some (left, top);
                    left_top = (new_left, new_top)}
          else d

let block_button_up (world : world_t) (x : float) (y : float) (d : disk_t) : disk_t =
  match d with
  | {prev = None} -> d
  | {left_top = (left, top);
     prev = Some (prev_left, prev_top);
     size = disk_size;
     bar = prev_bar;
     moved = mvd} ->
    let current_bar = on_which_bar x y in
    match fetch_bar_info world current_bar with
    | {height = h; disk_sizes = sizes} ->
      let top_size = List.fold_left (max) 0. sizes in
      if (prev_bar <> current_bar) &&                 (* 元の軸と違う軸で手を離した *)
         (current_bar <> 0) &&                        (* いずれかの軸の上(=背景ではない)で手を離した *)
         ((disk_size <= top_size) ||                  (* その軸の最小の輪より小さい、または *)
          (top_size = 0.))                            (* 軸にまだ１つも輪がない *)
      then
        {d with left_top = make_left_top current_bar (h +. 1.) disk_size;
                prev = None;
                moved = mvd + 1;
                pos = h +. 1.;
                bar = current_bar}
      else {d with left_top = (prev_left, prev_top);  (* 動かせなければ元の位置に戻す *)
                   prev = None}

let on_mouse_button_down (world : world_t) (x : float) (y : float) : world_t =
  match world with {mode = current_mode; disks = ds; msg = msg} -> 
    if on_reset_button x y then
      (if current_mode = Disk3 then initial_world_3 else initial_world_5)
    else if on_disk3_button x y then
      (if current_mode = Disk3 then world else initial_world_3)
    else if on_disk5_button x y then
      (if current_mode = Disk5 then world else initial_world_5)
    else
      {world with disks = List.map (block_button_down world x y) ds}

let on_mouse_button_up (world : world_t) (x : float) (y : float) : world_t =
  match world with {disks = ds} ->
    let new_disks = List.map (block_button_up world x y) ds in
    end_with {world with disks = new_disks}

let on_mouse (world : world_t) (x : float) (y : float) (mouse_event : string)
  : world_t =
  match mouse_event with
  | "button_down" | "drag" -> on_mouse_button_down world x y
  | "button_up" -> on_mouse_button_up world x y
  | _ -> {world with msg = "(" ^ (string_of_float x) ^ ", " ^ (string_of_float y) ^ ")"}

let _ =
  big_bang initial_world_3
    ~name:    "ToH"
    ~to_draw: draw
    ~width:   (int_of_float width)
    ~height:  (int_of_float height)
    ~on_mouse:on_mouse
