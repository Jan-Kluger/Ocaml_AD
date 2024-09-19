module GRAPH (HashTable: Hash_lib.Hash_sig.HASH_SIG) = struct
  type 'a vertex = 'a
  type weight = float
  type 'a neighbors = ('a vertex * weight) list  (* added wights to neigbors, howver this means that if i want to do a dag all weights should be inserted as 1, maybe ill make a seperate module for dag *)

  (* Use the type 't from the hash table signature *)
  type 'a graph = {
    adj_list: ('a vertex, 'a neighbors) HashTable.t;
    edges: (('a vertex * 'a vertex), weight) HashTable.t;
  }

  (* Add a vertex to the graph *)
  let add_vertex ~(graph: 'a graph) (v: 'a vertex) ~(hash_function: 'a vertex -> int) : 'a graph =
    if HashTable.contains ~hashtable:graph.adj_list v ~hash_function then
      graph
    else
      { graph with adj_list = HashTable.put ~hashtable:graph.adj_list (v, []) ~hash_function }

  (* Add an edge between v1 and v2 with a given weight *)
  let add_edge ~(graph: 'a graph) (v1: 'a vertex) (v2: 'a vertex) ~(weight: weight) ~(hash_function: 'a vertex -> int) : 'a graph =
    let neighbors = match HashTable.get ~hashtable:graph.adj_list v1 ~hash_function with
      | Some lst -> lst
      | None -> []
    in
    let updated_neighbors = if List.exists (fun (n, _) -> n = v2) neighbors then neighbors else (v2, weight) :: neighbors in (* also add weight*)
    let updated_adj_list = HashTable.put ~hashtable:graph.adj_list (v1, updated_neighbors) ~hash_function in
    let edge_key = (v1, v2) in
    let updated_edges = HashTable.put ~hashtable:graph.edges (edge_key, weight) ~hash_function:(fun (x, y) -> hash_function x + hash_function y) in
    { adj_list = updated_adj_list; edges = updated_edges }

  (* Find the weight of the edge between v1 and v2 *)
  let find_edge ~(graph: 'a graph) (v1: 'a vertex) (v2: 'a vertex) ~(hash_function: 'a vertex -> int) : weight option =
    let edge_key = (v1, v2) in
    HashTable.get ~hashtable:graph.edges edge_key ~hash_function:(fun (x, y) -> hash_function x + hash_function y)

  (* Remove an edge between v1 and v2 *)
  let remove_edge ~(graph: 'a graph) (v1: 'a vertex) (v2: 'a vertex) ~(hash_function: 'a vertex -> int) : 'a graph =
    let neighbors = match HashTable.get ~hashtable:graph.adj_list v1 ~hash_function with
      | Some lst -> List.filter (fun (n, _) -> n <> v2) lst
      | None -> []
    in
    let updated_adj_list = HashTable.put ~hashtable:graph.adj_list (v1, neighbors) ~hash_function in
    let edge_key = (v1, v2) in
    let updated_edges = HashTable.remove ~hashtable:graph.edges edge_key ~hash_function:(fun (x, y) -> hash_function x + hash_function y) in
    { adj_list = updated_adj_list; edges = updated_edges }

  (* Convert the graph to GraphViz DOT format with edge weights *)
  let to_graphviz ~(graph: 'a graph) ~(hash_function: 'a vertex -> int) ~(vertex_to_string: 'a vertex -> string) : string =
    let edges = ref [] in
    for i = 0 to HashTable.size graph.adj_list - 1 do
      match HashTable.get ~hashtable:graph.adj_list i ~hash_function with
      | Some neighbors ->
        List.iter (fun (n, _) -> (* iterate list, we now also need to include empty weight in argument*)
          match HashTable.get ~hashtable:graph.edges (i, n) ~hash_function:(fun (x, y) -> hash_function x + hash_function y) with
          | Some weight -> edges := !edges @ [Printf.sprintf "  %s -> %s [label=\"%f\"];" (vertex_to_string i) (vertex_to_string n) weight]
          | None -> ()
        ) neighbors
      | None -> ()
    done;
    let graph_body = String.concat "\n" !edges in
    "digraph G {\n" ^ graph_body ^ "\n}"
end
