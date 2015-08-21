let rec group_by (pred : 'a -> 'a -> bool) (l : 'a list) : 'a list list =
  let rec split_by (pred : 'a -> bool) (elems : 'a list) =
    match elems with
    | [] -> ([], [])
    | h :: t -> 
      let matching, not_matching = split_by pred t in
      if pred h then (h :: matching, not_matching) else (matching, h :: not_matching) in
  match l with
  | [] -> []
  | h :: t -> 
    let matching, not_matching = split_by (pred h) t in
    (h :: matching) :: (group_by pred not_matching);;

let anagram_eq a b = let sorted = List.sort compare in sorted a = sorted b;;
let pali_eq a b = (List.rev a) = b;
