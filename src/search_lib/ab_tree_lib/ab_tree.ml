type 'a tree = 
| Nil 
| Node of 'a * 'a list

module Ab_Tree : Search_lib.Search_sig.SEARCH with type 'a t = 'a tree = struct
  type 'a t = 'a tree

  let insert (avl_tree : 'a t) (element : 'a ) : 'a t = failwith "TODO"
  let remove (avl_tree : 'a t) (element : 'a ) : 'a t = failwith "TODO"
  let locate (avl_tree : 'a t) (element : 'a ) : 'a option = failwith "TODO"
  let toString  (avl_tree : 'a t) (to_string : ('a -> string)) : string = failwith "TODO"
  
end