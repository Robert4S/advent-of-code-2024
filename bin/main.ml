open Core
open! Aoc

module type Challenge_Intf = sig
  val result : string
  val day_num : int
end

let challenges : (module Challenge_Intf) list = [ (module Day1); (module Day2) ]

type challenge = (module Challenge_Intf)

let challenge_day (c : challenge) =
  let (module C) = c in
  C.day_num
;;

let challenge_result (c : challenge) =
  let (module C) = c in
  C.result
;;

let () =
  challenges
  |> List.iter ~f:(fun (c : challenge) ->
    Format.printf "Day %d:\n   %s\n" (challenge_day c) (challenge_result c))
;;
