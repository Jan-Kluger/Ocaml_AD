module BellmanFord (HashTable: Hash_lib.Hash_sig.HASH_SIG with type ('a, 'b) t = ('a * 'b) list array) = struct

  type 'a graph = {
    adj_list: ('a, ('a * float) list) HashTable.t;
  }

  let initialize_single_source ~(graph: 'a graph) ~(start: 'a) ~(hash_function: 'a -> int) : ('a, float) HashTable.t =
    let distance_table = HashTable.clear (Array.make (Array.length graph.adj_list) []) in
    let distance_table = HashTable.put ~hashtable:distance_table (start, 0.0) ~hash_function in
    distance_table

  let relax ~(distance_table: ('a, float) HashTable.t) (v1: 'a) (v2: 'a) (weight: float) ~(hash_function: 'a -> int) : ('a, float) HashTable.t =
    match HashTable.get ~hashtable:distance_table v1 ~hash_function with
    | Some dist_v1 -> (
        match HashTable.get ~hashtable:distance_table v2 ~hash_function with
        | Some dist_v2 ->
            if dist_v2 > dist_v1 +. weight then
              HashTable.put ~hashtable:distance_table (v2, dist_v1 +. weight) ~hash_function
            else
              distance_table
        | None -> distance_table
      )
    | None -> distance_table

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
      if count = 0 then distance_table
      else
        let distance_table = iterate_graph_adj_list graph (fun dist_tbl v1 v2 weight ->
          relax ~distance_table:dist_tbl v1 v2 weight ~hash_function
        ) distance_table
        in
        relax_edges distance_table (Array.length graph.adj_list - 1)
    in
    let distance_table = relax_edges distance_table (Array.length graph.adj_list - 1) in

    (* Step 2: Check for negative weight cycles *)
    iterate_graph_adj_list graph (fun has_cycle v1 v2 weight ->
      match HashTable.get ~hashtable:distance_table v1 ~hash_function, HashTable.get ~hashtable:distance_table v2 ~hash_function with
      | Some dist_v1, Some dist_v2 -> has_cycle || (dist_v2 > dist_v1 +. weight)
      | _ -> has_cycle
    ) false
end
