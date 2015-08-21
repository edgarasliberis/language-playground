let rec group_by (pred : 'a -> 'a -> bool) (l : 'a list) : 'a list list =
  (* Splits the list into two lists, 
     left with elements that satisfy the predicate,
     right with ones that don't *)
  let rec split_by (pred : 'a -> bool) (elems : 'a list) =
    match elems with
    | [] -> ([], [])
    | h :: t -> 
      let matching, not_matching = split_by pred t in
      if pred h then (h :: matching, not_matching) else (matching, h :: not_matching) in
  match l with
  | [] -> []
  | h :: t -> (* Get matching elements together and group the non-matching rest *)
    let matching, not_matching = split_by (pred h) t in
    (h :: matching) :: (group_by pred not_matching);;

let anagram_eq a b = let sorted = List.sort compare in sorted a = sorted b;;
let pali_eq a b = (List.rev a) = b;;

let rec nth_in_merge ?(cmp = compare) l1 l2 n  =
  match l1 with
  | [] -> List.nth l2 n
  | x :: xs -> 
    match l2 with
    | [] -> List.nth l1 n
    | y :: ys ->
      if cmp x y < 0 then 
        if n = 0 then x else nth_in_merge xs l2 (n - 1) ~cmp
      else 
        if n = 0 then y else nth_in_merge l1 ys (n - 1) ~cmp;;

(** 
  Reverses arr in the range [i; j]
 *)
let reverse_arr arr i j =
  for k = 0 to (j - i) / 2 do 
    let tmp = arr.(i + k) in
    arr.(i + k) <- arr.(j - k);
    arr.(j - k) <- tmp
  done;;

let rot_left arr n =
  let len = Array.length arr in
  let n = n mod len in
  if n == 0 then ()
  else 
    reverse_arr arr 0 (n-1);
    reverse_arr arr n (len-1);
    reverse_arr arr 0 (len-1);;

(* Get's the best element in the list according to some metric
   eval:    provide an evaluation function, that would assign score to the elements
            (higher is better)
   l:       list of elements to choose from, has to contain at least one item
   returns: element with the highest score, along with the score value 
 *)
let get_best (eval: 'b -> 'a) (l : 'b list) =
  let update (curr : ('a * 'b)) (next : 'b) : ('a * 'b) =
    let curr_score, curr_elem = curr in
    let next_score = eval next in
    if compare curr_score next_score < 0 then (next_score, next) else curr in
  match l with
  | [] -> failwith "no elements provided"
  | b :: rest -> List.fold_left update (eval b, b) rest;;

let sum_list = List.fold_left ( + ) 0;;
let len_list = List.length;;

