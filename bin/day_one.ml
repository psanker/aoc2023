open Base
module Char_trie = Trie.Make (Char)

let tee ~func lst =
  lst
  |> List.map ~f:(fun x ->
    func x;
    x)
;;

let explode_string str = String.to_list str, List.rev @@ String.to_list str

let rec part_one inp =
  let num =
    inp
    |> List.map ~f:explode_string
    |> List.map ~f:digits
    |> List.map ~f:Int.of_string
    |> List.fold ~init:0 ~f:(fun acc x -> x + acc)
  in
  Stdlib.print_endline @@ Printf.sprintf "%d" num

and digits (forward, backward) =
  let d_1, d_n = take_first_digit forward, take_first_digit backward in
  Printf.sprintf "%c%c" d_1 d_n

and take_first_digit str_l =
  str_l |> List.filter ~f:Char.is_digit |> List.hd |> Option.value_exn
;;

let rec part_two inp =
  let forward_map =
    [ "one", '1'
    ; "two", '2'
    ; "three", '3'
    ; "four", '4'
    ; "five", '5'
    ; "six", '6'
    ; "seven", '7'
    ; "eight", '8'
    ; "nine", '9'
    ; "zero", '0'
    ]
  in
  let backward_map = forward_map |> List.map ~f:(fun (k, v) -> String.rev k, v) in
  let forward_trie = Char_trie.create None in
  let backward_trie = Char_trie.create None in
  let _ =
    List.map forward_map ~f:(fun (word, num) ->
      Char_trie.set forward_trie (String.to_list word) num)
  in
  let _ =
    List.map backward_map ~f:(fun (word, num) ->
      Char_trie.set backward_trie (String.to_list word) num)
  in
  let num =
    inp
    |> List.map ~f:explode_string
    |> List.map ~f:(fun (f, b) ->
      dewordify ~trie:forward_trie f, dewordify ~trie:backward_trie b)
    |> List.map ~f:digits
    |> List.map ~f:Int.of_string
    |> List.fold ~init:0 ~f:(fun acc x -> x + acc)
  in
  Stdlib.print_endline @@ Printf.sprintf "%d" num

and dewordify ~trie char_l =
  let flush ~onto buffer = List.fold ~init:onto ~f:(fun acc x -> x :: acc) buffer in
  let rec dewordify' acc buffer char_sl =
    let sub_trie = Char_trie.sub trie (List.rev buffer) in
    let value_opt = Char_trie.get trie (List.rev buffer) in
    match sub_trie, buffer, char_sl with
    | _, [], [] ->
      let acc = acc |> List.rev in
      Stdlib.print_endline
      @@ Printf.sprintf "In: %s; Out: %s" (String.of_list char_l) (String.of_list acc);
      acc
    | Some node, _, x :: xs when Char_trie.is_leaf node ->
      dewordify' (Option.value_exn value_opt :: acc) [ x ] xs
    | Some node, _, [] when Char_trie.is_leaf node ->
      dewordify' (Option.value_exn value_opt :: acc) [] []
    | Some _, buf, x :: xs -> dewordify' acc (x :: buf) xs
    | None, [], x :: xs -> dewordify' acc [ x ] xs
    | None, ([ _ ] as buf), x :: xs -> dewordify' (flush ~onto:acc buf) [ x ] xs
    | None, ([ _ ] as buf), [] -> dewordify' (flush ~onto:acc buf) [] []
    | None, h_buf :: buf, xs ->
      dewordify' (buf |> List.rev |> flush ~onto:acc) [ h_buf ] xs
    | _, buf, [] -> dewordify' (buf |> List.rev |> flush ~onto:acc) [] []
  in
  dewordify' [] [] char_l
;;

let run () =
  let inp1 = Advent.read_lines "input/day_one.prod" in
  let inp2 = Advent.read_lines "input/day_one.prod" in
  let _ = part_one inp1 in
  let _ = part_two inp2 in
  ()
;;
