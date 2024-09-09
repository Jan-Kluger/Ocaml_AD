type 'a tree = 
| Nil 
| Node of ('a * int * 'a tree * 'a tree)

module Avl_tree : Search_lib.Search_sig.SEARCH with type 'a t = 'a tree = struct
  type 'a t = 'a tree

  let height (node : 'a tree) : int = match node with
    | Nil -> 0
    | Node (_, h, _, _) -> h

  let calculate_balance (node : 'a tree) = match node with
    | Nil -> 0
    | Node (_, _, l, r) -> (height l) - (height r)

  (* let rec calculate_height (node : 'a tree) : int = match node with
    | Nil -> 0
    | Node (_, _, l, r) -> 1 + max (height l) (height r) *)


let rotate_right (node : 'a tree) : 'a tree = match node with
| Node (z, _, Node (y, _, t1, t2), t3) ->
    let new_left = Node (z, 1 + max (height t2) (height t3), t2, t3) in
    Node (y, 1 + max (height t1) (height new_left), t1, new_left)
| _ -> node

let rotate_left (node : 'a tree) : 'a tree = match node with
| Node (z, _, t1, Node (y, _, t2, t3)) ->
    let new_right = Node (z, 1 + max (height t1) (height t2), t1, t2) in
    Node (y, 1 + max (height new_right) (height t3), new_right, t3)
| _ -> node

let rebalance (node : 'a tree) : 'a tree =
let balance = calculate_balance node in
if balance > 1 then
  match node with
  | Node (_, _, l, _) ->
      if calculate_balance l < 0 then
        let rotated_left = rotate_left l in
        let new_node = match node with
          | Node (v, _, _, r) -> Node (v, height node, rotated_left, r)
          | _ -> failwith "Invalid node structure"
        in
        rotate_right new_node
      else
        rotate_right node
  | _ -> node
else if balance < -1 then
  match node with
  | Node (_, _, _, r) ->
      if calculate_balance r > 0 then
        let rotated_right = rotate_right r in
        let new_node = match node with
          | Node (v, _, l, _) -> Node (v, height node, l, rotated_right)
          | _ -> failwith "Invalid node structure"
        in
        rotate_left new_node
      else
        rotate_left node
  | _ -> node
else
  node

let rec insert (avl_tree : 'a tree) (element : 'a) : 'a tree = match avl_tree with
| Nil -> Node (element, 1, Nil, Nil)  (* New node with height 1 *)
| Node (value, _, l, r) ->
    if element < value then
      let new_left = insert l element in
      let new_node = Node (value, 1 + max (height new_left) (height r), new_left, r) in
      rebalance new_node  (* Rebalance after insertion *)
    else if element > value then
      let new_right = insert r element in
      let new_node = Node (value, 1 + max (height l) (height new_right), l, new_right) in
      rebalance new_node  (* Rebalance after insertion *)
    else
      avl_tree  (* Element already exists, no changes *)

  (* let rec insert (avl_tree : 'a t) (element : 'a ) : 'a t = match avl_tree with
  | Nil -> Node (element, 0, Nil, Nil)
  | Node (value, _, l, r) -> (
    if value > element then
      insert l element
    else
      insert r element
    ) *)

(* Helper function to find the minimum value node in a subtree *)
let rec find_min (node : 'a tree) : 'a tree = match node with
  | Nil -> failwith "Tree is empty"
  | Node (_, _, Nil, _) -> node
  | Node (_, _, l, _) -> find_min l

let rec remove_min (node : 'a tree) : 'a tree = match node with
  | Nil -> failwith "Tree is empty"
  | Node (_, _, Nil, r) -> r
  | Node (v, _, l, r) -> 
      let new_left = remove_min l in
      let updated_node = Node (v, 1 + max (height new_left) (height r), new_left, r) in
      rebalance updated_node

let rec remove (avl_tree : 'a tree) (element : 'a) : 'a tree = match avl_tree with
  | Nil -> Nil
  | Node (value, _, l, r) -> 
      if element < value then
        let new_left = remove l element in
        let updated_node = Node (value, 1 + max (height new_left) (height r), new_left, r) in
        rebalance updated_node
      else if element > value then
        let new_right = remove r element in
        let updated_node = Node (value, 1 + max (height l) (height new_right), l, new_right) in
        rebalance updated_node
      else
        match (l, r) with
        | (Nil, Nil) -> Nil
        | (Nil, _) -> r
        | (_, Nil) -> l
        | (_, _) -> (
            let successor = find_min r in
            match successor with
          | Node (successor_value, _, _, _) ->
              let new_right = remove_min r in
              let updated_node = Node (successor_value, 1 + max (height l) (height new_right), l, new_right) in
              rebalance updated_node
          | Nil -> failwith "Baum, impossible :P"
        )

  let rec locate (avl_tree : 'a t) (element : 'a ) : 'a option = match avl_tree with
  | Nil -> None
  | Node (value, _, l, r) ->
      if element = value then
        Some value
      else if element < value then
        locate l element
      else
        locate r element

  let rec toString (avl_tree : 'a t) (to_string : ('a -> string)) : string = match avl_tree with
  | Nil -> "Nil"
  | Node (value, height, left, right) ->
    let left_str = toString left to_string in
    let right_str = toString right to_string in
    "Node(" ^ to_string value ^ ", " ^ string_of_int height ^ ", " ^ left_str ^ ", " ^ right_str ^ ")"
  
end