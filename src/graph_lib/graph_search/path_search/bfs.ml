open Graph_lib.Graph
module ChainHashGraph = GRAPH(Chain_hash_lib.Chain_hash.CHAIN_HASH)

module BFS (HashTable: Hash_lib.Hash_sig.HASH_SIG with type ('a, 'b) t = ('a * 'b) list array)
  : Graph_search_sig_lib.Graph_search_sig.SEARCH with type 'a graph = 'a ChainHashGraph.graph = struct
  
  type 'a graph = 'a ChainHashGraph.graph

    (* here, we reconstruct the shortest path from one node to another using our hash table, recursivley going to the stored parent node *)
  let rec reconstruct_path parent_map current path hash_function =
    match HashTable.get ~hashtable:parent_map current ~hash_function with
    | None -> List.rev (current :: path)
    | Some parent -> reconstruct_path parent_map parent (current :: path) hash_function

    (* main search function *)
  let search ~(graph: 'a graph) ~(start: 'a) ~(dest: 'a) ~(hash_function: 'a -> int) : 'a list option =
    (* prepare empty hash table for visited nodes *)
    let visited = HashTable.clear (Array.make (Array.length graph.adj_list) []) in
    (* prepare empty hash tablle for parents, these are just from where the 
    shortest connection is coming from so we  can reconstruct a graph later *)
    let parent_map = HashTable.clear (Array.make (Array.length graph.adj_list) []) in
    (* simple queue *)
    let queue = Queue.create () in

    (* queue opur startting position and add it to visited *)
    Queue.add start queue;
    let visited = HashTable.put ~hashtable:visited (start, true) ~hash_function in

    (* Main BFS fucntion *)
    let rec bfs_loop visited parent_map =
      if Queue.is_empty queue then
        None
      else
        (* under the condition that the queue is not empty we set our current node from our queue *)
        let current = Queue.take queue in
        (* if we have found our destination we reconstruct path to node through parents *)
        if current = dest then
          Some (reconstruct_path parent_map current [] hash_function)  (* Destination found *)
        else
          match HashTable.get ~hashtable:graph.adj_list current ~hash_function with
          | None -> bfs_loop visited parent_map  (* No neighbors, continue BFS *)
          | Some neighbors ->
            let visited, parent_map =
            List.fold_left (fun (visited, parent_map) (neighbor, _) ->  (* Ignore weight with `_` *)
            if HashTable.contains ~hashtable:visited neighbor ~hash_function then
              (visited, parent_map)  (* Already visited *)
            else
              (* if node has neightbors that ahvnt been visited, then add neighbors to queue *)
                (* add to visitetd and add the node and where we got the from to the parent map *)
                  let visited = HashTable.put ~hashtable:visited (neighbor, true) ~hash_function in
                  let parent_map = HashTable.put ~hashtable:parent_map (neighbor, current) ~hash_function in
                  (* queue neighor *)
                  Queue.add neighbor queue;
                  (visited, parent_map)
              ) (visited, parent_map) neighbors
            in
            (* loop *)
            bfs_loop visited parent_map
    in
    (* start loop *)
    bfs_loop visited parent_map
end
