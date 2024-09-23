
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

(* Problem 5 *)
(* Author: Joaquin Nietes*)

let l4 = [0;1;1;3;4];;

let rec binNumbers list =
  match list with 
  | [] -> 0
  | 1 :: tl -> binNumbers tl + 1
  | 0 :: tl -> binNumbers tl + 1
  | hd :: tl -> binNumbers tl
;;

binNumbers l4;;

(* Problem 7 *)
(* Author: Joaquin Nietes*)

(* nr = choose(n-1, r-1) + choose(n-1, r) *)
(* base case: choose(n, 0) = choose(n, n) = 1 *)
let rec choose n r = 
  match n, r with
  | n, 0 -> 1
  | n, r when n = r -> 1
  | _ -> choose (n-1) (r-1) + choose (n-1) r
;;

choose 10 5;;

(* Problem 8 *)
(* Author: Joaquin Nietes*)
let rec dup list = 
  match list with
  | [] -> []
  | [hd] -> [hd;hd]
  | hd :: tl -> hd :: hd :: dup tl
;;

let l5 = ['a';'b';'c';'d'];;
let l6 = ['a'];;

dup l5;;
dup l6;;

(* Problem 11 *)
(* Author: Joaquin Nietes*)
type btree =
  | Empty
  | Leaf of float
  | Node of (float * btree * btree)

let btree1 = Node(3.1, Node(0.9, Empty, Leaf(2.5)), Leaf(10.2))