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

  let rec calculate_height (node : 'a tree) : int = match node with
    | Nil -> 0
    | Node (_, _, l, r) -> 1 + max (height l) (height r)

  let rec insert (avl_tree : 'a t) (element : 'a ) : 'a t = match avl_tree with
  | Nil -> Node (element, 0, Nil, Nil)
  | Node (value, _, l, r) -> (
    if value > element then
      insert l element
    else
      insert r element
    )

  let remove (avl_tree : 'a t) (element : 'a ) : 'a t = failwith "TODO"
  let locate (avl_tree : 'a t) (element : 'a ) : 'a = failwith "TODO"
  let toString  (avl_tree : 'a t) (to_string : ('a -> string)) : string = failwith "TODO"
  
end