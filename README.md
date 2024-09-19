# Algorithms & Data structures in Ocaml
Practicing Algorithms and datastructures in purely functional OCaml. 

This project contains
- Stacks
- Queues
- Quicksort
- Mergsort
- Binary Heap
- Avl Trees
- ab Trees
- Hashing with chaining
- Hashing with linear probing
- Graphs
- BFS
- DFS

Working on:
- Dijkstras
- Bellman-Ford
- Floyd-Warshall
- Johnson-Algorithm
- Kruskal
- Prim

# Program structure:
```
└── Ocaml_AD/
    └── src/
        └── test/
            ├── test_Stack_test_2.ml
        └── bin/
            ├── main.ml
        └── hash_lib/
            ├── hash_sig.ml
            └── chain_hash/
                ├── chain_hash.ml
            └── hash_lin_prob/
                ├── hash_lin_prob.ml
        └── queue_lib/
            ├── queue_sig.ml
            ├── list_queue.ml
            ├── queue_sig.mli
        └── search_lib/
            ├── search_sig.mli
            ├── search_sig.ml
            └── avl_tree_lib/
                ├── avl_tree.ml
            └── ab_tree_lib/
                ├── ab_tree.ml
        └── sa_lib/
            ├── sorted_array_sig.ml
            ├── sorted_array_list.ml
            ├── sorted_array_sig.mli
        └── stack_lib/
            ├── stack_sig.mli
            ├── stack_sig.ml
            ├── list_stack.ml
        └── sort_lib/
            ├── list_mergesort.ml
            ├── sort_sig.mli
            ├── list_quicksort.ml
            ├── sort_sig.ml
        └── graph_lib/
            ├── graph.ml
            └── graph_search/
                ├── graph_search_sig.ml
                ├── positive_min_graph_sig.ml
                └── path_search/
                    ├── bfs.ml
                    ├── dfs.ml
                └── positive_min/
                    ├── dijkstras.ml
        └── prio_queue_lib/
            ├── prioq_sig.ml
            ├── prioq_sig.mli
            └── priority_queue/
                ├── list_prioq.ml
            └── heap/
                ├── heap_imp.ml
```
