module BellmanFord (HashTable: Hash_lib.Hash_sig.HASH_SIG with type ('a, 'b) t = ('a * 'b) list array) = struct

  type 'a graph = {
    adj_list: ('a, ('a * float) list) HashTable.t;
  }

  (* Initalize an emty hash table with the starting node having weight 0.0 *)
  let initialize_single_source ~(graph: 'a graph) ~(start: 'a) ~(hash_function: 'a -> int) : ('a, float) HashTable.t =
    (* The hashtable.clear method, takes any hash table, here an (a,b) array list and maps every enetry of the table to an empty list *)
    let distance_table = HashTable.clear (Array.make (Array.length graph.adj_list) []) in
    (* We insert the starting node in to the hash table *)
    let distance_table = HashTable.put ~hashtable:distance_table (start, 0.0) ~hash_function in
    (* Return the table *)
    distance_table
    
(* This is the function that we use to update a weight *)
  let relax ~(distance_table: ('a, float) HashTable.t) (v1: 'a) (v2: 'a) (weight: float) ~(hash_function: 'a -> int) : ('a, float) HashTable.t =
    (* We fetch both nodes from the hash table, if any of them are not contained, we return the unupdated hash table *)
    match HashTable.get ~hashtable:distance_table v1 ~hash_function with
    | Some dist_v1 -> (
        match HashTable.get ~hashtable:distance_table v2 ~hash_function with
        | Some dist_v2 ->
          (* if bot are found we check if going from v1 + the weight of the edge is more efficient, if it is, update the weight and return *)
            if dist_v2 > dist_v1 +. weight then
              HashTable.put ~hashtable:distance_table (v2, dist_v1 +. weight) ~hash_function
            else
              (* else we dont update the weight and return *)
              distance_table
        | None -> distance_table
      )
    | None -> distance_table

    (* The is a helper function to parse the hash table since we cant fold left on the hash table itself *)
  let iterate_graph_adj_list (graph: 'a graph) f acc : 'b =
    Array.fold_left (fun acc bucket ->
      List.fold_left (fun acc (v1, neighbors) ->
        List.fold_left (fun acc (v2, weight) ->
          f acc v1 v2 weight
        ) acc neighbors
      ) acc bucket
    ) acc graph.adj_list

  (* Bellman-Ford algorithm implementation *)
  let has_negative_cycle ~(graph: 'a graph) ~(start: 'a) ~(hash_function: 'a -> int) : bool =
    let distance_table = initialize_single_source ~graph ~start ~hash_function in

    (* Step 1: Relax all edges |V| - 1 times *)
    let rec relax_edges distance_table count : ('a, float) HashTable.t =
      (* If the count reaches 0, stop and return distance table *)
      if count = 0 then distance_table
      else
        (* Iterate over all edges in the graph and apply the relaxation step to update distances *)
        let distance_table = iterate_graph_adj_list graph (fun dist_tbl v1 v2 weight ->
          relax ~distance_table:dist_tbl v1 v2 weight ~hash_function
        ) distance_table
        in
        relax_edges distance_table (Array.length graph.adj_list - 1)
    in
    (* Recursively call relax_edges, decreasing the count each time to simulate |V| - 1 passes *)
    let distance_table = relax_edges distance_table (Array.length graph.adj_list - 1) in

    (* Step 2: Check for negative weight cycles *)
    (* After |V| - 1 passes, if we can still relax any edge, there must be a negative weight cycle. *)
    iterate_graph_adj_list graph (fun has_cycle v1 v2 weight ->
      match HashTable.get ~hashtable:distance_table v1 ~hash_function, HashTable.get ~hashtable:distance_table v2 ~hash_function with
      | Some dist_v1, Some dist_v2 -> has_cycle || (dist_v2 > dist_v1 +. weight)
              (* If we can further relax the distance from v1 to v2, that implies a negative weight cycle *)
      | _ -> has_cycle
    ) false
end
