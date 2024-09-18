open Graph_lib.Graph
module ChainHashGraph = GRAPH(Chain_hash_lib.Chain_hash.CHAIN_HASH)

module BFS (HashTable: Hash_lib.Hash_sig.HASH_SIG) : Graph_search_sig_lib.Graph_search_sig.SEARCH with type 'a graph = 'a ChainHashGraph.graph = struct
  type 'a graph = 'a ChainHashGraph.graph

  let search ~(graph: 'a graph) ~(start: 'a) ~(dest: 'a) ~(hash_function: 'a -> int) : 'a list option =
    failwith "TODO"
end
