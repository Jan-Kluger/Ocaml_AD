(*  *)
module List_Quicksort : Sort_sig.SORT_SIG with type 'a t = 'a list = struct
  type 'a t = 'a list

    let rec sort (list : 'a t) (comparator :('a -> 'a -> int)) : 'a t = match list with
    | [] -> []
    | x::xs ->
        let smaller, larger = List.partition (fun y -> comparator y x < 0) xs in
        sort smaller comparator @ (x :: sort larger comparator)

    let toString (s_array : 'a t) (to_string_method : ('a -> string)) = 
      let rec toString_helper (s_array : 'a t) (acc : string) (to_string_method : ('a -> string)) = 
        match s_array with
        | h :: [] -> toString_helper [] (acc^(to_string_method h)^"]") to_string_method
        | h :: t -> toString_helper t (acc^(to_string_method h)^", ") to_string_method
        | [] -> acc
    in
    if s_array == [] then
      toString_helper (s_array) "[]" to_string_method
    else
      toString_helper (s_array) "[" to_string_method

  (* let rec take_in_range (list : 'a t) (start_index : int) (end_index : int) : 'a list =
    match list, start_index, end_index with
    | _ :: t, x, _ when x > 0 -> take_in_range t (start_index - 1) (end_index - 1)
    | h :: t, 0, x when x > 0 -> h :: take_in_range t (start_index) (end_index - 1)
    | _ :: _, _, _ -> []
    | _, _, _ -> []
    
  let sort (list : 'a t) (comparator :('a -> 'a -> int)) : 'a t = (take_in_range list 0 3) @ (take_in_range list 4 (List.length list))

  let swap (list : 'a t) (i : int) (j : int) : 'a t = 
    if i == j then
      list
    else if not (0 <= i && i < List.length list) then failwith "index i out of bounds"
    else if not (0 <= j && j < List.length list) then failwith "index j out of bounds"
    else list *)

  end

let list_to_take = [1; 2; 3; 4; 5; 6; 7; 8; 9]

let int_comparator x y = compare x y

let sorted_list = List_Quicksort.sort list_to_take int_comparator

let () = print_endline (List_Quicksort.toString sorted_list string_of_int)
