module type POSITIVE_MIN_GRAPH = sig
  type 'a graph

  val shortest_paths :
    graph:'a graph -> 
    start:'a -> 
    hash_function:('a -> int) -> 
    ('a * float) list * ('a * 'a option) list
    (* The return type is a list of distances, each node of type 'a is associated with its shortest distance from the start node*)
    (* The other tuple list is a list of preesessors, which makes it easy to reconsturct the shortest path *)
end