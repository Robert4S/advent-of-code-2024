open Core

module Gradient = struct
  type t =
    [ `ascending
    | `descending
    ]
  [@@deriving eq]

  let ( = ) = equal
end

let difference a b = if a > b then a - b else b - a

let has_gradient list =
  let rec aux (gradient : Gradient.t) prev list =
    match list with
    | [] -> true
    | x :: xs when Gradient.(gradient = `ascending) && x > prev -> aux `ascending x xs
    | x :: xs when Gradient.(gradient = `descending) && x < prev -> aux `descending x xs
    | _ -> false
  in
  match list with
  | x :: xs -> aux `ascending x xs || aux `descending x xs
  | _ -> true
;;

let has_differences list =
  let rec aux prev list =
    match list with
    | [] -> true
    | x :: xs
      when let d = difference x prev in
           d >= 1 && d <= 3 -> aux x xs
    | _ -> false
  in
  match list with
  | [] -> true
  | x :: xs -> aux x xs
;;

let rec powerset = function
  | [] -> [ [] ]
  | x :: xs ->
    let ps = powerset xs in
    ps @ List.map ~f:(fun ss -> x :: ss) ps
;;

let sublists list =
  let len = List.length list in
  let all_subs = powerset list in
  List.filter all_subs ~f:(fun a -> List.length a = len - 1)
;;

let is_safe list = has_gradient list && has_differences list

let is_safe_dampened list =
  let sublists = sublists list in
  List.find sublists ~f:is_safe |> Option.is_some
;;

let day_num = 2
let safe_count = Day2_input.input |> List.filter ~f:is_safe |> List.length

let safe_dampened_count =
  Day2_input.input |> List.filter ~f:is_safe_dampened |> List.length
;;

let result =
  Format.sprintf "Challenge 1: %d\n   Challenge 2: %d" safe_count safe_dampened_count
;;
