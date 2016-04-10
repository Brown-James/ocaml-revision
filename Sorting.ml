(* Quick sort *)
let rec filter f xs = match xs with
  | [] -> []
  | x :: xs -> if f x then x :: filter f xs else filter f xs
								       
let rec quick_sort = function
  | [] -> []
  | hd :: tl -> let prefix = quick_sort (filter (fun n -> n < hd) tl) in
	       let suffix = quick_sort (filter (fun n -> n >= hd) tl) in
	       prefix @ (hd :: suffix)

(* Merge sort *)

(* Bubble sort *)

(* Insertion sort *)

(* Selection sort *)

(* Checking *)
let rec sorted = function
  | [] -> true
  | x :: [] -> true
  | x :: x' :: xs -> if x < x' then (true && sorted xs) else false
