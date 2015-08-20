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
      List.flatten (List.map (insert_everywhere h) (permutations t))
end