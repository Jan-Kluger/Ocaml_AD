module type SEARCH = sig
  type 'a graph

  val search : graph:'a graph -> start:'a -> dest:'a -> 'a list option
end
