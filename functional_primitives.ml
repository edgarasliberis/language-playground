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

let func a = a + 1;;

type 'a llnodeelement = None | Some of ('a * ('a llnode))
 and 'a llnode = 'a llnodeelement ref;;

class ['a] linked_list limit = 
  object (self)
    val list = ref None

    method remove_last = 
      let rec loop (l : 'a llnode)  = 
        match !l with
        | None -> failwith "can't remove element from an empty list"
        | Some (_, nref) -> begin
          match !nref with
          | None -> l := None
          | Some (_, _) -> loop nref
        end in
      loop list

    method add elem =
      list := Some (elem, list);
      size := !size + 1

    method conditional_add elem =
      if !size > limit then self#remove_last;
      self#add elem

    method take pred =
      let rec loop (l : 'a llnode) =
        match !l with 
        | None -> None
        | Some (v, next) -> if (pred v) then  else loop next in
      loop list
  end;;

(* Memoisation wrapper utilising LinkedList for LRU caching *)
module MemoList = struct 
  let memoize f = 
    let memos = new linked_list 20 in
    let handle arg = 
      match memos#take (fun (k, v) -> k = arg) with
      | None -> let v = f arg in memos#conditional_add v; v  
      | Some (k, v) -> v
    handle

end
