
(* Problem 1 *)
(* Author: Joaquin Nietes*)

let l1 = [];;
let l2 = [5];;
let l3 = [3; 1; 4; 5; 9];;

let rec listStatus list =
  match list with
  | [] -> 1
  | [hd] -> 2
  | hd :: tl -> 0
;;

listStatus l1;;
listStatus l2;;
listStatus l3;;

(* Problem 3 *)
(* Author: Joaquin Nietes*)

let rec pop list = 
  match list with
  | [] -> []
  | [hd] -> [hd]
  | hd :: tl -> pop tl
;;

pop l1;;
pop l2;;
pop l3;;