module type BellmanFord = sig
    type 'a graph
  
    val has_negative_cycle : graph: ('a graph) -> start: ('a) -> hash_function: ('a -> int) -> bool
end 