type 'a tree = 
| Nil 
| Node of 'a * 'a tree * 'a tree

module Heap_imp : Prioq_lib.Prioq_sig.PRIORITY_QUEUE with type 'a t = 'a tree = struct

  type 'a t = 'a tree
  
  let tree_to_list (p_queue : 'a tree) : 'a option list =
    let rec bfs queue list =
      match queue with
      | [] -> list
      | Nil :: rest -> bfs rest (list @ [None])
      | Node (value, left, right) :: rest ->
          bfs (rest @ [left; right]) (list @ [Some value])
    in
    bfs [p_queue] []

  let find_last_in_list (lst : 'a option list) =
    let rec find_last idx =
      if idx < 0 then (None, lst)
      else match List.nth lst idx with
        | Some v -> (Some v, List.mapi (fun i x -> if i = idx then None else x) lst)
        | None -> find_last (idx - 1)
    in
    find_last (List.length lst - 1)

  let list_to_tree (lst : 'a option list) : 'a t =
    let rec build_tree idx =
      if idx >= List.length lst then Nil
      else match List.nth lst idx with
        | None -> Nil
        | Some value -> 
          let left = build_tree (2 * idx + 1) in
          let right = build_tree (2 * idx + 2) in
          Node (value, left, right)
    in
    build_tree 0
    
  let rec sift_down (p_queue : 'a t) (comp : ('a -> 'a -> bool)) : 'a t = 
    let check_right q v1 l r =
      match r with
      | Nil -> q
      | Node (val3, left3, right3) -> 
        if (comp v1 val3) then
          (Node (val3, l, sift_down (Node(v1, left3, right3)) comp))
        else
          q
        in
    match p_queue with
    | Nil -> p_queue
    | Node(val1, left, right) -> 
      match left with
      | Node (val2, left2, right2) -> (
        if (comp val1 val2) then 
          (Node (val2, sift_down (Node(val1, left2, right2)) comp, right))
        else 
          check_right p_queue val1 left right
      )
        | Nil -> 
          check_right p_queue val1 left right

  let insert (_ : 'a t) (_ : 'a) (_ : ('a -> 'a -> bool)) : 'a t = failwith "TODO"

  let build (elements : 'a list) (comp : ('a -> 'a -> bool)) : 'a t = 
    List.fold_left (fun acc el -> insert acc el comp) Nil elements

  let sift_up (_ : 'a t) (_ : 'a ) (_ : ('a -> 'a -> bool)) : 'a t = failwith "TODO"
    
  let min (p_queue : 'a t) : 'a option = match p_queue with
  | Nil -> None
  | Node (value, _, _) -> Some value

  let delete_min (p_queue : 'a t) (comp : ('a -> 'a -> bool)) : 'a t option = 
    let (temp, new_list) = find_last_in_list (tree_to_list p_queue) in
    let new_tree = list_to_tree new_list in
    let new_queue =
    match new_tree, temp with
    | Node (_, left, right), Some num -> Some (Node (num, left, right))
    | _ -> None
    in
    match new_queue with
    | Some baum -> Some (sift_down baum comp)
    | None -> None

  let rec to_string (p_queue : 'a t) (to_str : 'a -> string) : string =
    match p_queue with
    | Nil -> "Nil"
    | Node (value, left, right) ->
      let left_str = to_string left to_str in
      let right_str = to_string right to_str in
      "Node(" ^ to_str value ^ ", " ^ left_str ^ ", " ^ right_str ^ ")"

end