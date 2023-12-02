open Base

let max_values = 12, 13, 14

let split_once ~on str =
  match String.split ~on str with
  | [] -> [], []
  | [ x ] -> [ x ], []
  | x :: xs -> [ x ], xs
;;

let hd_exn lst = Option.value_exn @@ List.hd lst

module Game = struct
  type t =
    { id : int
    ; counts : int * int * int
    }

  let id t = t.id

  let rec of_string str =
    let game_str, rounds_strs = split_once ~on:':' str in
    let id =
      game_str
      |> hd_exn
      |> String.to_list
      |> List.filter ~f:Char.is_digit
      |> String.of_list
      |> Int.of_string
    in
    { id; counts = parse_rounds rounds_strs }

  and parse_rounds round_strs =
    round_strs
    |> hd_exn
    |> String.split ~on:';'
    |> List.map ~f:parse_round
    |> List.fold ~init:(0, 0, 0) ~f:(fun (acc_r, acc_g, acc_b) (x_r, x_g, x_b) ->
      Int.max acc_r x_r, Int.max acc_g x_g, Int.max acc_b x_b)

  and parse_round round_str =
    round_str
    |> String.strip
    |> String.split ~on:','
    |> List.map ~f:String.strip
    |> List.map ~f:parse_draw
    |> List.fold ~init:(0, 0, 0) ~f:(fun (acc_r, acc_g, acc_b) (x_r, x_g, x_b) ->
      acc_r + x_r, acc_g + x_g, acc_b + x_b)

  and parse_draw draw =
    let count =
      draw
      |> String.to_list
      |> List.filter ~f:Char.is_digit
      |> String.of_list
      |> Int.of_string
    in
    let color =
      draw
      |> String.to_list
      |> List.filter ~f:Char.is_alpha
      |> String.of_list
      |> String.lowercase
    in
    match color with
    | "red" -> count, 0, 0
    | "green" -> 0, count, 0
    | "blue" -> 0, 0, count
    | _ -> 0, 0, 0
  ;;

  let is_valid t =
    match t.counts, max_values with
    | (r, _, _), (r', _, _) when r > r' -> false
    | (_, g, _), (_, g', _) when g > g' -> false
    | (_, _, b), (_, _, b') when b > b' -> false
    | _ -> true
  ;;

  let power t =
    match t.counts with
    | r, g, b -> r * g * b
  ;;
end

let part_one inp =
  let all_ids =
    inp
    |> List.map ~f:Game.of_string
    |> List.filter ~f:Game.is_valid
    |> List.map ~f:Game.id
    |> List.fold ~init:0 ~f:( + )
  in
  Stdlib.print_endline @@ Printf.sprintf "%d" all_ids
;;

let part_two inp =
  let all_ids =
    inp
    |> List.map ~f:Game.of_string
    |> List.map ~f:Game.power
    |> List.fold ~init:0 ~f:( + )
  in
  Stdlib.print_endline @@ Printf.sprintf "%d" all_ids
;;

let run () =
  let inp1 = Advent.read_lines "input/day_two.prod" in
  let inp2 = Advent.read_lines "input/day_two.prod" in
  part_one inp1;
  part_two inp2;
;;

