module Graph (HashTable: Hash_lib.Hash_sig.HASH_SIG) = struct
  type vertex = int
  type weight = float
  type neighbors = vertex list

  (* Use the type 't from the hash table sig *)
  type graph = {
    adj_list: (vertex, neighbors) HashTable.t;
    edges: (vertex * vertex, weight) HashTable.t;
  }

  (* Add a vertex to the graph *)
  let add_vertex ~(graph: graph) (v: vertex) ~(hash_function: vertex -> int) : graph =
    if HashTable.contains ~hashtable:graph.adj_list v ~hash_function then
      graph
    else
      { graph with adj_list = HashTable.put ~hashtable:graph.adj_list (v, []) ~hash_function }

  (* Add edge between v1 and v2*)
  let add_edge ~(graph: graph) (v1: vertex) (v2: vertex) ~(weight: weight) ~(hash_function: vertex -> int) : graph =
    let neighbors = match HashTable.get ~hashtable:graph.adj_list v1 ~hash_function with
      | Some lst -> lst
      | None -> []
    in
    let updated_neighbors = if List.mem v2 neighbors then neighbors else v2 :: neighbors in
    let updated_adj_list = HashTable.put ~hashtable:graph.adj_list (v1, updated_neighbors) ~hash_function in
    let edge_key = (v1, v2) in
    let updated_edges = HashTable.put ~hashtable:graph.edges (edge_key, weight) ~hash_function:(fun (x, y) -> hash_function x + hash_function y) in
    {adj_list = updated_adj_list; edges = updated_edges }

  (* Find weight of edge between v1 and v2 *)
  let find_edge ~(graph: graph) (v1: vertex) (v2: vertex) ~(hash_function: vertex -> int) : weight option =
    let edge_key = (v1, v2) in
    HashTable.get ~hashtable:graph.edges edge_key ~hash_function:(fun (x, y) -> hash_function x + hash_function y)

  (* Remove an edge between v1 and v2 *)
  let remove_edge ~(graph: graph) (v1: vertex) (v2: vertex) ~(hash_function: vertex -> int) : graph =
    let neighbors = match HashTable.get ~hashtable:graph.adj_list v1 ~hash_function with
      | Some lst -> List.filter ((<>) v2) lst
      | None -> []
    in
    let updated_adj_list = HashTable.put ~hashtable:graph.adj_list (v1, neighbors) ~hash_function in
    let edge_key = (v1, v2) in
    let updated_edges = HashTable.remove ~hashtable:graph.edges edge_key ~hash_function:(fun (x, y) -> hash_function x + hash_function y) in
    {adj_list = updated_adj_list; edges = updated_edges }

  (* Convert to GraphViz*)
  let to_graphviz (graph: graph) ~(hash_function: vertex -> int) : string =
    let edges = ref [] in
    for i = 0 to HashTable.size graph.adj_list - 1 do
      match HashTable.get ~hashtable:graph.adj_list i ~hash_function with
      | Some neighbors ->
        List.iter (fun n ->
          match HashTable.get ~hashtable:graph.edges (i, n) ~hash_function:(fun (x, y) -> hash_function x + hash_function y) with
          | Some weight -> edges := !edges @ [Printf.sprintf "  %d -> %d [label=\"%f\"];" i n weight]
          | None -> ()
        ) neighbors
      | None -> ()
    done;
    let graph_body = String.concat "\n" !edges in
    "digraph G {\n" ^ graph_body ^ "\n}"
end