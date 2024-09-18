module type SEARCH = sig
  type 'a graph

  val search : graph:'a graph -> start:'a -> dest:'a -> hash_function:('a -> int) -> 'a list option
end
