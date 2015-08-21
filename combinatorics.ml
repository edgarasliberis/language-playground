module Combinatorics = struct
  let rec permutations (l : 'a list) : ('a list) list =
    let prepend_to_all (e : 'a) (p : ('a list) list) = List.map (fun xs -> e :: xs) p in
    let rec insert_everywhere (e : 'a) (p : 'a list) =
      match p with
      | [] -> [[e]]
      | h :: t -> (e :: p) :: (prepend_to_all h (insert_everywhere e t)) in
    match l with
    | [] -> [[]]
    | h :: t -> (* Could do reverse flattening for efficiency *)
      List.flatten (List.map (insert_everywhere h) (permutations t));;

  let pairs (elems : 'a list) : ('a * 'a) list list =
    let es = Array.of_list elems in
    let n = Array.length es in
    let make_pair day a_idx = (es.(a_idx), es.((a_idx + day) mod n)) in
    let iterate_pairs func times = 
      let pair_list = ref [] in
      for i = 0 to times - 1 do 
        pair_list := func i :: !pair_list done; 
      !pair_list in
    let result = ref [] in
    for day = 1 to n-1 do 
      result := (iterate_pairs (make_pair day) (n / 2)) :: !result
    done;
    !result;;

  let rec combinations (k : int) (l : 'a list) : 'a list list =
    let prepend_to_all (e : 'a) (p : ('a list) list) = List.map (fun xs -> e :: xs) p in
    if k = 0 then [[]] else
      match l with
      | [] -> []
      | h :: t -> List.rev_append (combinations k t) (prepend_to_all h (combinations (k - 1) t));;
end