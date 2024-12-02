open Core
open Day1_input

let l1_sorted = Int.(List.sort ~compare l1)
let l2_sorted = Int.(List.sort ~compare l2)
let zipped = List.zip_exn l1_sorted l2_sorted
let distances = List.map zipped ~f:(fun (a, b) -> if a >= b then a - b else b - a)
let distance = distances |> List.fold ~init:0 ~f:Int.( + )

let scale_number from num =
  let ocurrences = List.filter from ~f:(fun a -> a = num) in
  let scalar = List.length ocurrences in
  num * scalar
;;

let scaled_l1 = List.map l1_sorted ~f:(scale_number l2_sorted)
let scaled_l2 = List.map l2_sorted ~f:(scale_number l1_sorted)
let l1_score = List.fold scaled_l1 ~init:0 ~f:Int.( + )
let l2_score = List.fold scaled_l2 ~init:0 ~f:Int.( + )
let result = Format.sprintf "Challenge 1: %d,\n   Challenge 2: %d" distance l1_score
let day_num = 1
