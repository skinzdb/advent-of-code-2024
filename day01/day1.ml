(*
let read_file filename =
let lines = ref [] in
let chan = open_in filename in
try
  while true; do
    lines := input_line chan :: !lines
  done; !lines
with End_of_file ->
  close_in chan;
  List.rev !lines ;;

let part1 =
    let contents = read_file "data.txt" in
    let parsed = List.map (fun x -> List.filter (fun y -> y <> "") (String.split_on_char ' ' x)) contents in
    List.map (fun row -> print_endline (List.fold_left (fun acc x -> x ^ ", " ^ acc) "" row)) parsed;
    let xs = List.map (fun x -> int_of_string (List.hd x)) parsed in
    let ys = List.map (fun y -> int_of_string (List.hd (List.tl y))) parsed in
    let dist = List.map2 (fun x y -> abs (x - y)) xs ys in
    List.fold_right (+) dist 0;;

let () = print_endline part1;;
*)
