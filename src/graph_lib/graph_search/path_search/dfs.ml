open Graph_lib.Graph
module ChainHashGraph = GRAPH(Chain_hash_lib.Chain_hash.CHAIN_HASH)

module DFS (HashTable: Hash_lib.Hash_sig.HASH_SIG with type ('a, 'b) t = ('a * 'b) list array)
  : Graph_search_sig_lib.Graph_search_sig.SEARCH with type 'a graph = 'a ChainHashGraph.graph = struct

  type 'a graph = 'a ChainHashGraph.graph

  (* For more extensive comments see BFS, basically the same thing but here we use a stack *)

  let rec reconstruct_path parent_map current path hash_function =
    match HashTable.get ~hashtable:parent_map current ~hash_function with
    | None -> List.rev (current :: path)
    | Some parent -> reconstruct_path parent_map parent (current :: path) hash_function

  let search ~(graph: 'a graph) ~(start: 'a) ~(dest: 'a) ~(hash_function: 'a -> int) : 'a list option =
    let visited = HashTable.clear (Array.make (Array.length graph.adj_list) []) in
    let parent_map = HashTable.clear (Array.make (Array.length graph.adj_list) []) in
    let stack = Stack.create () in

    Stack.push start stack;

    let visited = HashTable.put ~hashtable:visited (start, true) ~hash_function in

    let rec dfs_loop visited parent_map =
      if Stack.is_empty stack then
        None  (* Destination not found *)
      else
        let current = Stack.pop stack in
        if current = dest then
          Some (reconstruct_path parent_map current [] hash_function)  (* Destination found *)
        else
          match HashTable.get ~hashtable:graph.adj_list current ~hash_function with
          | None -> dfs_loop visited parent_map  (* No neighbors, continue DFS *)
          | Some neighbors ->
            let visited, parent_map =
              List.fold_left (fun (visited, parent_map) (neighbor, _) ->  (* Ignore weight with `_` *)
                if HashTable.contains ~hashtable:visited neighbor ~hash_function then
                  (visited, parent_map)  (* Already visited *)
                else
                  let visited = HashTable.put ~hashtable:visited (neighbor, true) ~hash_function in
                  let parent_map = HashTable.put ~hashtable:parent_map (neighbor, current) ~hash_function in
                  Stack.push neighbor stack;
                  (visited, parent_map)
              ) (visited, parent_map) neighbors
            in
            dfs_loop visited parent_map
    in
    dfs_loop visited parent_map
end
