(* Derrick Pavia
   Assignment01
   Problems: Evens *)

(* Problem 2 Rotate list removing the head and placing it at the end *)
(* Derrick *)

let rotate x = 
  match x with
  | [] -> [] (* If list is empty return empty list*)
  | head :: tail -> tail @ [head];; (* Split into head and tail concatenate head to the tail *)
let a = [1;2;3;];;
rotate a;;

(* Problem 4 Remove all elements y from a list can use if and else statements *)
(* Derrick *)

let rec remove y x_list =
  match x_list with
  | [] -> [] (* Empty list return empty  *)
  | head :: tail ->
      if head = y then (* If the head is equal to y remove y and return tail *)
        remove y tail
      else (* Else return the head concatenated with the tail with y removed *)
        head :: remove y tail;;
let a = [1;2;3;];;
remove 2 a;;

(* Problem 6 makepairs that take list x and a making pairs of (x1,a1) (x2,a2) etc *)
(* Derrick *)

let rec makepairs x y =
  match (x, y) with
  | ([],[]) -> [] (* Both lists empty return empty list *)
  | ([], _) -> y (* First list empty return second list *)
  | (_, []) -> x (* Second list empty return first list *)
  | (head_x :: tail_x, head_y :: tail_y) -> (* Split into head and tail for x and for y *)
      head_x :: head_y :: makepairs tail_x tail_y;; (* Recursivly gives tails to makepairs and adds their heads to new list *)
let x = [1;3;5;];;
let y =[2;4;6;];;
makepairs y x;;

(* Problem 8 dup function that takes a list [a;b;c;] and returns [a;a;b;b;c;c] *)
(* Derrick *)

let rec dup x = 
  match x with
  | [] -> raise (Failure "Empty list") (* Empty list return failure *)
  | head :: tail -> 
      head :: head :: dup tail;; (* Gives new list the head twice and recursivly gives it new tail and does it again until their is no tail *)
let x = [1;2;3;];;
dup x;;

(* Problem 10 function that returns the smallest num in a list, hint use min *)
(* Derrick *)

let rec smallest x =
  match x with
  | [] -> raise (Failure "Empty list")
  | [x] -> x (* if there is only one element return it *)
  | hd :: tl -> (* Split into head and tail *)
      List.fold_left min hd tl;; (* Compare the head element to tail returning the min and passing it recursivly back into the function *)
let x = [3;2;1;];;
smallest x;;

(* Problem 11 *)
(* Author: Joaquin Nietes*)
type btree =
  | Empty
  | Leaf of float
  | Node of (float * btree * btree)

let btree1 = Node(3.1, Node(0.9, Empty, Leaf(2.5)), Leaf(10.2))

(* Problem 12 a function that returns the depth of a binary tree of floats *)
(* Derrick *)

let rec depth tree = 
  match tree with
  | Empty -> 0 (* Empty tree return 0 *)
  | Leaf _ -> 1 (* Leaf returns 1 depth *)
  | Node (_, left, right) ->
      1 + max (depth left) (depth right);;
let z = depth btree1;;