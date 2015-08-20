(* Memoisation wrapper utilising Hashtbl, no cache control *)
module Memoization = struct
  let memoize f = 
    let memos = Hashtbl.create 1 in
    let handle arg = 
      try Hashtbl.find memos arg 
      with Not_found -> 
        let result = f arg in
        Hashtbl.add memos arg result;
        result in
    handle
end

let func a = print_string "func was called!"; a + 1;;

type 'a llnodeelement = Nil | Elem of ('a * ('a llnode))
 and 'a llnode = 'a llnodeelement ref;;

class ['a] linked_list limit = 
  object (self)
    val mutable list = ref Nil
    val size = ref 0

    method get_first_node () = !list

    method remove_last () = 
      let rec loop (l : 'a llnode)  = 
        match !l with
        | Nil -> failwith "can't remove element from an empty list"
        | Elem (_, nref) -> begin
          match !nref with
          | Nil -> l := Nil
          | Elem (_, _) -> loop nref
        end in
      loop list

    method add (elem : 'a) =
      list <- ref (Elem (elem, list));
      size := !size + 1

    method conditional_add (elem : 'a) =
      if !size > limit then self#remove_last ();
      self#add elem

    method remove (l : 'a llnode) =
      match !l with
      | Nil -> failwith "node is an empty list"
      | Elem (_, child) -> (* copy l's child into l *)
        l := !child; 

    method take (pred : 'a -> bool) : 'a option =
      let rec loop (l : 'a llnode) : 'a option =
        match !l with 
        | Nil -> None
        | Elem (v, next) -> if (pred v) then (self#remove l; Some v) else loop next in
      loop list
  end;;

(* Memoisation wrapper utilising LinkedList for LRU caching *)
module MemoList = struct 
  let memoize (f : 'a -> 'b) = 
    let memos = new linked_list 5 in
    let handle (arg : 'a) : 'b = 
      match memos#take (fun (k, v) -> k = arg) with
      | None -> let v = f arg in (memos#conditional_add (arg, v); v)
      | Some (_, v) -> (memos#add (arg, v); v) in
    handle
end


module Primitives = struct
  let rec foldl (f : 'a -> 'b -> 'b) (l : 'a list) (init : 'b) : 'b =
    match l with
    | [] -> init
    | h :: t -> foldl f t (f h init);;

  let reverse (l : 'a list) : 'a list =
    let rec loop (xs : 'a list) (accum : 'a list) = 
      match xs with
      | [] -> accum
      | h :: t -> loop t (h :: accum) in
    loop l [];;

  let rec map (f : 'a -> 'b) (l : 'a list) : 'b list =
    match l with
    | [] -> []
    | h :: t -> f h :: map f t;;

  let map_rev_accum (f : 'a -> 'b) (l : 'a list) : 'b list =
    let rec loop (xs : 'a list) (accum : 'a list) = 
      match xs with
      | [] -> accum
      | h :: t -> loop t (f h :: accum) in
    loop l [];;

  let map_f (f : 'a -> 'b) (l : 'a list) : 'b list =
    foldl (fun x xs -> f x :: xs) l [];;

  let rec filter (pred : 'a -> bool) (l : 'a list) : 'a list =
    match l with
    | [] -> []
    | h :: t when pred h -> h :: filter pred t
    | h :: t -> filter pred t;;

  let filter_rev_f (pred : 'a -> bool) (l : 'a list) : 'a list =
    foldl (fun x xs -> if pred x then x :: xs else xs) l [];;

  let rec foldr (f : 'a -> 'b -> 'b) (l : 'a list) (init : 'b) : 'b =
    match l with
    | [] -> init
    | h :: t -> f h (foldr f t init);;

  let rec foldr_cps (f : 'a -> 'b -> 'b) (l : 'a list) (init : 'b) (continue_with : 'b -> 'c): 'c =
    let f_cps = fun a b cont -> cont (f a b) in
    match l with
    | [] -> continue_with init
    | h :: t -> foldr_cps f t init (fun res -> f_cps h res continue_with);;
end
