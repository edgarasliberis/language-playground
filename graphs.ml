module Graph = struct
  (* Adjacency list *)
  type t = int list array;;

  let init numNodes = Array.create numNodes [];;

  (* Adds i -> j to g *)
  let add_edge (g : t) (i : int) (j : int) =
    if not (List.mem j g.(i)) then g.(i) <- j :: g.(i);;

  let dfs (g : t) (source : int) =
    let visited : bool array = Array.create (Array.length g) false in
    let rec loop (i : int) =
      visited.(i) <- true;
      print_endline @@ string_of_int i;
      List.iter (fun j -> 
        if not visited.(j) then loop j
      ) g.(i) in
    loop source;;

  let bfs (g : t) (source : int) =
    let visited : bool array = Array.create (Array.length g) false in
    let queue = Queue.create () in
    Queue.push source queue;
    while not (Queue.is_empty queue) do
      let i = Queue.pop queue in
      if not visited.(i) then (
        print_endline @@ string_of_int i;
        visited.(i) <- true;
        List.iter (fun j -> 
          Queue.push j queue
        ) g.(i)
      )
    done
end

(* Small test graph *)
let g = Graph.init 5;;
Graph.add_edge g 0 1;;
Graph.add_edge g 1 2;;
Graph.add_edge g 2 0;;
Graph.add_edge g 2 3;;
Graph.add_edge g 2 4;;
Graph.add_edge g 3 4;;
Graph.add_edge g 1 3;;
