module BellmanFord (_: Hash_lib.Hash_sig.HASH_SIG with type ('a, 'b) t = ('a * 'b) list array) : sig
    type 'a graph
  
    val has_negative_cycle : graph: ('a graph) -> start: ('a) -> hash_function: ('a -> int) -> bool
end 