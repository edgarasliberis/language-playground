(* All data structures presented here are mutable *)

module SillyPriorityQueue = struct
  type 'a pqueue = {mutable queue : 'a list; mutable tqueue : 'a list };;
  let create () = { queue = []; tqueue = [] };;

  let normalise q =
    let rec insert l item =
      match l with
      | [] -> [item]
      | h :: t -> 
        if compare item h <= 0 then item :: l
        else h :: (insert t item) in
    q.queue <- List.fold_left insert q.queue q.tqueue;
    q.tqueue <- [];;

  let pop q = 
    (if q.tqueue != [] || q.queue = [] then normalise q);
    match q.queue with
    | [] -> failwith "can't pop from an empty queue"
    | h :: t -> q.queue <- t; h;;

  let push q item =
    q.tqueue <- item :: q.tqueue;;

end

module LinkedList = struct 
  type 'a llnode = { mutable value : 'a; mutable next : 'a llnode option }
  type 'a llist = 'a llnode option ref

  let create () : 'a llist = ref None;;

  let push (l : 'a llist) (elem : 'a) : unit =
    l := Some {value = elem; next = !l};;

  let top (l : 'a llist) : 'a =
    match !l with
    | None -> failwith "supplied list is empty"
    | Some {value = v; next = _} -> v;;

  let pop (l : 'a llist) : unit =
    match !l with
    | None -> failwith "supplied list is empty"
    | Some {value = _; next = next} -> l := next;;

  let find (l : 'a llist) (elem : 'a) : 'a llist =
    let rec look (l : 'a llnode option) =
      match l with
      | None -> failwith "given element was not found"
      | Some {value = v; next = _} when v = elem -> ref l
      | Some {value = _; next = n} -> look n in
    look !l;;

  let delete (l : 'a llist) (elem : 'a) = 
    let rec loop curr parent =
      match curr with
      | None -> failwith "element was not found!"
      | Some {value = v; next = n} when v = elem ->
        (* Element was found in the current node *)
        let Some p = parent in (* We know its 'Some' *)
        p.next <- n; (* Remove the current node *)
      | Some {value = _; next = n} -> loop n curr in
    match !l with
    | None -> failwith "can't delete from an empty list!"
    | Some {value = v; next = _} when v = elem -> pop l
    | Some {value = _; next = n} -> loop n !l;;

  let fold_left (f : 'b -> 'a -> 'b) (init : 'b) (l : 'a llist) : 'b =
    let rec loop (l : 'a llnode option) (accum : 'b) =
      match l with
      | None -> accum
      | Some {value = v; next = n} -> loop n (f accum v) in
    loop !l init;;
end

module ArrayHeap = struct
  type 'a heap = {mutable size : int; mutable data : 'a array};;

  let singleton (elem : 'a) : 'a heap = {size = 1; data = Array.make 5000 elem};;

  (* with which branch swap was made *)
  type heapify_result = Left | Right | Neither;;

  let swap h i j =
    let tmp = h.data.(i) in
    h.data.(i) <- h.data.(j);
    h.data.(j) <- tmp;;

  let heapify_node h i : heapify_result =
    let l_child = 2 * i + 1 in
    let r_child = 2 * i + 2 in
    if l_child >= h.size then (* leaf node *) Neither
    else if r_child >= h.size then begin
      (* one child - left, which is a leaf node *)
      (if h.data.(l_child) < h.data.(i) then swap h i l_child); Left 
    end
    else begin
      (* two children *)
      if h.data.(l_child) < h.data.(i) && h.data.(l_child) < h.data.(r_child) then (
        swap h i l_child;
        Left
      ) else if h.data.(r_child) < h.data.(i) && h.data.(r_child) < h.data.(l_child) then (
        swap h i r_child;
        Right
      ) else Neither
    end

  let rec heapify_down (h : 'a heap) (i : int) : unit =
    match heapify_node h i with
    | Left -> heapify_down h (2 * i + 1)
    | Right -> heapify_down h (2 * i + 2)
    | Neither -> ();;

  let rec heapify_up (h : 'a heap) (i : int) : unit =
    let parent = (i - 1) / 2 in
    match heapify_node h i with
    | Left | Right -> if i != 0 then heapify_up h parent else ()
    | Neither -> ();;

  let push (h : 'a heap) (elem : 'a) : unit =
    h.data.(h.size) <- elem;
    h.size <- h.size + 1;
    let parent = (h.size - 2) / 2 in
    heapify_up h parent;;

  let top (h : 'a heap) : 'a =
    match h with
    | {size = 0; data = _} -> failwith "empty heap"
    | {size = _; data = data} -> data.(0)

  let pop (h : 'a heap) : unit =
    swap h 0 (h.size - 1);
    h.size <- h.size - 1;
    heapify_down h 0;;

end


module BST = struct
  type 'a t = Leaf | Br of 'a * ('a t) * ('a t);;

  let create () : 'a t = Leaf;;

  let rec insert (t : 'a t) (elem : 'a) : 'a t = 
    match t with
    | Leaf -> Br (elem, Leaf, Leaf)
    | Br (v, l, r) -> if elem < v then Br(v, insert l elem, r) else Br(v, l, insert r elem);;

  let rec take_largest (t : 'a t) : ('a * 'a t) =
    match t with
    | Leaf -> failwith "empty tree"
    | Br (v, l, Leaf) -> (v, l)
    | Br (v, l, r) ->
      let (lrg, rem) = take_largest r in
      (lrg, Br(v, l, rem));;

  let delete_root (t : 'a t) : 'a t =
    match t with
    | Leaf -> failwith "can't delete from an empty tree"
    | Br (_, Leaf, Leaf) -> Leaf
    | Br (_, Leaf, r) -> r
    | Br (_, l, Leaf) -> l
    | Br (_, l, r) ->
      let (lrg, rem) = take_largest l in
      Br (lrg, rem, r);;

  let rec traverse (f : 'b -> 'a -> 'b -> 'b) (init : 'b) (t : 'a t) : 'b =
    match t with
    | Leaf -> init
    | Br (v, l, r) -> f (traverse f init l) v (traverse f init r);;

  let count = traverse (fun l -> fun v -> fun r -> l + 1 + r) 0;;
  let sum = traverse (fun l -> fun v -> fun r -> l + v + r) 0;;
  let inorder = traverse (fun l -> fun v -> fun r -> l @ [v] @ r) [];;
  let preorder = traverse (fun l -> fun v -> fun r -> v :: (l @ r)) [];;
  let postorder = traverse (fun l -> fun v -> fun r -> l @ r @ [v]) [];;
  let maxdepth = traverse (fun l -> fun v -> fun r ->  1 + max l r) 0;;
  let nth (t : 'a t) = List.nth (inorder t);; (* Inefficient, creates the whole list *)
  let map (t : 'a t) (f : 'a -> 'b) =
    traverse (fun l -> fun v -> fun r -> Br(f v, l, r)) Leaf t;;  

end
