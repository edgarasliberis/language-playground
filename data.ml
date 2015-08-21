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
    (if q.tqueue != [] or q.queue = [] then normalise q);
    match q.queue with
    | [] -> failwith "can't pop from an empty queue"
    | h :: t -> q.queue <- t; h;;

  let push q item =
    q.tqueue <- item :: q.tqueue;;

end