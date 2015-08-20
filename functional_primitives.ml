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
