(* open Stack_lib.List_stack
open Queue_lib.List_queue
open Sa_lib.Sorted_array_list
open Sort_lib.List_mergesort *)
open Avl_tree_lib.Avl_tree

let () = print_endline "stack_str"
(* let int_comparator x y = (compare x y)>0 *)

let avl_tree = Nil

let avl_tree = Avl_tree.insert avl_tree 5

let avl_tree = Avl_tree.insert avl_tree 8

let avl_tree = Avl_tree.insert avl_tree 2

let avl_tree = Avl_tree.insert avl_tree 3

let avl_tree = Avl_tree.insert avl_tree 4

let () = print_endline (Avl_tree.toString avl_tree string_of_int)

(* let heap = Heap_imp.insert heap 7 int_comparator

let heap = Heap_imp.delete_min heap int_comparator *)

(* let heap_to_print = 
  match heap with
  | Some tree -> tree
  | _ -> failwith "No tree to print" *)


(* let to_print = 
match test_val with
| Node (value, _, _) -> value
| _ -> failwith "this shouldnt happen" *)

(* let () =
  print_endline (Heap_imp.to_string heap_to_print string_of_int) *)



(* let int_comparator x y = compare x y


let list_to_sort = [1; 4; 5; 2; 3]
let sorted_list = List_mergesort.sort list_to_sort int_comparator


let sa_list = Sorted_array_list.insert [] 1 int_comparator
let sa_list = Sorted_array_list.insert sa_list 10 int_comparator
let sa_list = Sorted_array_list.insert sa_list 7 int_comparator
let sa_list = Sorted_array_list.insert sa_list 8 int_comparator
let sa_list = Sorted_array_list.insert sa_list 3 int_comparator
let sa_list = Sorted_array_list.insert sa_list 9 int_comparator


let queue = List_queue.push_back [] 10

let stack = List_stack.push [] 10
let stack = List_stack.push stack 20
let stack = List_stack.push stack 30

let () = 
print_endline (Sorted_array_list.toString sa_list string_of_int);;

let () = 
print_endline (List_stack.toString stack string_of_int)

let () = 
print_endline (List_queue.toString queue string_of_int)

let () = 
print_endline (List_mergesort.toString sorted_list string_of_int) *)