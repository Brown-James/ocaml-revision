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
let rec merge xs ys = match xs, ys with
  | [], []       -> []
  | x::xs', []    -> x::xs'
  | [], y::ys'    -> y::ys'
  | x::xs', y::ys' -> if x < y then x :: (merge xs' (y::ys'))
		      else y :: (merge (x::xs') ys')

let split xs =
  let rec aux (l, r) = function
    | []       -> (l, r)
    | x::[]    -> (x::l, r)
    | x::y::xs -> aux (x::l, y::r) xs
  in aux ([], []) xs

let rec mergesort xs = let left, right = split xs in
		       match left, right with
		       | [], [] -> []
		       | x::xs, [] -> (x::xs)
		       | [], y::ys -> (y::ys)
		       | x::xs, y::ys -> merge (mergesort (x::xs)) (mergesort (y::ys))

(* Bubble sort *)

(* Insertion sort *)

(* Selection sort *)

(* Checking *)
let rec sorted = function
  | [] -> true
  | x :: [] -> true
  | x :: x' :: xs -> if x < x' then (true && sorted xs) else false
