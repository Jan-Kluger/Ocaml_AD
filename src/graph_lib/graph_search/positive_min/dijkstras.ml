open Graph_lib.Graph

type 'a tree = 
  | Nil 
  | Node of 'a * 'a tree * 'a tree

module ChainHashGraph = GRAPH(Chain_hash_lib.Chain_hash.CHAIN_HASH)

module Dijkstra 
  (Heap : Prioq_lib.Prioq_sig.PRIORITY_QUEUE with type 'a t = 'a tree) 
  (HashTable : Hash_lib.Hash_sig.HASH_SIG with type ('a, 'b) t = ('a * 'b) list array) 
  : Graph_search_sig_lib.Positive_min_graph_sig.POSITIVE_MIN_GRAPH with type 'a graph = 'a ChainHashGraph.graph = struct

  type 'a graph = 'a ChainHashGraph.graph

  (* Helper function to extract the shortest path from the parent map *)
  let rec reconstruct_path parent_map current path =
    match HashTable.get ~hashtable:parent_map current ~hash_function:(fun x -> Hashtbl.hash x) with
    | None -> List.rev (current :: path)
    | Some parent -> reconstruct_path parent_map parent (current :: path)

  (* Dijkstra's algorithm implementation *)
  let shortest_paths ~(graph: 'a graph) ~(start: 'a) ~(hash_function: 'a -> int) =
    (* Initialize the distance table and parent map *)
    let distance_table = HashTable.clear (Array.make (Array.length graph.adj_list) []) in
    let parent_map = HashTable.clear (Array.make (Array.length graph.adj_list) []) in
    let visited = HashTable.clear (Array.make (Array.length graph.adj_list) []) in

    (* Set the distance to the start node as 0 *)
    let distance_table = HashTable.put ~hashtable:distance_table (start, 0.0) ~hash_function in
    let parent_map = HashTable.put ~hashtable:parent_map (start, None) ~hash_function in

    (* Priority queue implemented with a binary heap using a binary tree structure *)
    let heap = Heap.build [(0.0, start)] (fun (a, _) (b, _) -> a < b) in

    (* Dijkstra's loop *)
    let rec dijkstra_loop heap distance_table parent_map visited =
      match Heap.min heap with
      | None -> 
          (* Convert hash tables into lists for final return *)
          let distances = Array.fold_left (fun acc bucket ->
            List.fold_left (fun acc' (node, dist) -> (node, dist) :: acc') acc bucket
          ) [] distance_table in
          let predecessors = Array.fold_left (fun acc bucket ->
            List.fold_left (fun acc' (node, parent) -> (node, parent) :: acc') acc bucket
          ) [] parent_map in
          (distances, predecessors)  (* Return lists of distances and predecessors *)

      | Some (current_distance, current_node) ->
        let heap = Option.get (Heap.delete_min heap (fun (a, _) (b, _) -> a < b)) in
        if HashTable.contains ~hashtable:visited current_node ~hash_function then
          dijkstra_loop heap distance_table parent_map visited  (* Skip already visited nodes *)
        else
          let visited = HashTable.put ~hashtable:visited (current_node, true) ~hash_function in

          (* Process neighbors *)
          match HashTable.get ~hashtable:graph.adj_list current_node ~hash_function with
          | None -> dijkstra_loop heap distance_table parent_map visited  (* No neighbors *)
          | Some neighbors ->
            let (heap, distance_table, parent_map) =
              List.fold_left (fun (heap, distance_table, parent_map) (neighbor, weight) ->
                let new_distance = current_distance +. weight in
                match HashTable.get ~hashtable:distance_table neighbor ~hash_function with
                | None ->
                  (* New neighbor, add with updated distance and parent *)
                  let distance_table = HashTable.put ~hashtable:distance_table (neighbor, new_distance) ~hash_function in
                  let parent_map = HashTable.put ~hashtable:parent_map (neighbor, Some current_node) ~hash_function in
                  let heap = Heap.insert heap (new_distance, neighbor) ~comp:(fun (a, _) (b, _) -> a < b) in
                  (heap, distance_table, parent_map)
                | Some old_distance ->
                  if new_distance < old_distance then
                    (* Update neighbor with shorter path *)
                    let distance_table = HashTable.put ~hashtable:distance_table (neighbor, new_distance) ~hash_function in
                    let parent_map = HashTable.put ~hashtable:parent_map (neighbor, Some current_node) ~hash_function in
                    let heap = Heap.insert heap (new_distance, neighbor) ~comp:(fun (a, _) (b, _) -> a < b) in
                    (heap, distance_table, parent_map)
                  else
                    (heap, distance_table, parent_map)
              ) (heap, distance_table, parent_map) neighbors
            in
            dijkstra_loop heap distance_table parent_map visited
    in
    dijkstra_loop heap distance_table parent_map visited
end
