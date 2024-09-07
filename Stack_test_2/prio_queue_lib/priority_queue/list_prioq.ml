type 'a tree = 
| Nil 
| Node of 'a * 'a tree * 'a tree

module List_prioq : Prioq_lib.Prioq_sig.PRIORITY_QUEUE with type 'a t = 'a tree = struct
  type 'a t = 'a tree

  let rec insert (p_queue : 'a t) (element : 'a) (comp : ('a -> 'a -> bool)) : 'a t =
    match p_queue with
    | Nil -> Node (element, Nil, Nil)
    | Node (value, left, right) -> 
      if element < value then
        Node (value, (insert left element comp), right)
      else
        Node (value, left, (insert right element comp))

  let build (elements : 'a list) (comp : ('a -> 'a -> bool)) : 'a t = 
    List.fold_left (fun tree element -> insert tree element comp) Nil elements

  let min (p_queue : 'a t) : 'a option = match p_queue with
  | Nil -> None
  | Node (value, _, _) -> Some value

  let rec merge (t1 : 'a tree) (t2 : 'a tree) : 'a tree = match t1, t2 with
    | Nil, t -> t
    | t, Nil -> t
    | Node (v1, l1, r1), Node (v2, l2, r2) ->
      if v1 <= v2 then
        Node (v1, r1, merge l1 t2)
      else
        Node (v2, r2, merge t1 l2)

  let delete_min (p_queue : 'a t) (_ : ('a -> 'a -> bool)) : 'a t option = match p_queue with
    | Nil -> None
    | Node (_, left, right) -> Some (merge left right)

    let rec to_string (p_queue : 'a t) (to_str : 'a -> string) : string =
      match p_queue with
      | Nil -> "Nil"
      | Node (value, left, right) ->
        let left_str = to_string left to_str in
        let right_str = to_string right to_str in
        "Node(" ^ to_str value ^ ", " ^ left_str ^ ", " ^ right_str ^ ")"

end