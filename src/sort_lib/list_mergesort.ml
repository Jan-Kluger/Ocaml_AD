module List_mergesort : Sort_sig.SORT_SIG with type 'a t = 'a list = struct
  type 'a t = 'a list

  let rec merge (one : 'a t) (two : 'a t) (comparator : 'a -> 'a -> int) = 
    match one, two with
    | xs, [] -> xs
    | [], ys -> ys   
    | x :: xs, y :: ys -> if comparator x y <= 0 then
      x :: merge xs two comparator
    else
      y :: merge one ys comparator

  let rec take (list : 'a t) (num : int) : 'a t=
    match list, num with
    | _, 0 | [], _ -> []
    | h :: t, n -> h :: take t (n-1)

  let rec drop (list : 'a t) (num : int) : 'a t=
    match list, num with
    | _, 0 | [], _ -> list
    | _ :: t, n -> drop t (n-1)

    let rec sort (list  : 'a t) (comparator : 'a -> 'a -> int) : 'a t = 
    match list with
    | [] | [_] -> list  (* Base case: empty list or single element list *)
    | _ ->
      let half_list_size = (List.length list) / 2 in
      let left = take list half_list_size in
      let right = drop list half_list_size in
      merge (sort left comparator) (sort right comparator) comparator

  let toString (s_array : 'a t) (to_string_method : ('a -> string)) = 
    let rec toString_helper (s_array : 'a t) (acc : string) (to_string_method : ('a -> string)) = 
      match s_array with
      | h :: [] -> toString_helper [] (acc^(to_string_method h)^"]") to_string_method
      | h :: t -> toString_helper t (acc^(to_string_method h)^", ") to_string_method
      | [] -> acc
  in
  toString_helper (List.rev s_array) "[" to_string_method

end
(* 
module List_mergesort  = struct
  type 'a t = 'a list

let rec take (list : 'a t)  (num : int): 'a t=
  match list, num with
  | _, 0 | [], _ -> []
  | h :: t, n -> h :: take t (n-1)

let rec drop (list : 'a t)  (num : int): 'a t=
  match list, num with
  | _, 0 | [], _ -> list
  | _ :: t, n -> drop t (n-1)

let rec merge (one : 'a t) (two : 'a t) comparator = 
  match one, two with
  | xs, [] -> xs
  | [], ys -> ys   
  | x :: xs, y :: ys -> if comparator x y <= 0 then
    x :: merge xs two comparator
  else
    y :: merge one ys comparator

  let rec sort (list  : 'a t) (comparator : 'a -> 'a -> int) : 'a t = 
    let (half_list_size : int) = (List.length list)/2 in
    let (left : 'a t) = take list half_list_size in
    let (right : 'a t) = drop list half_list_size in
    merge (sort left comparator) (sort right comparator) comparator

let toString (s_array : 'a t) (to_string_method : ('a -> string)) = 
  let rec toString_helper (s_array : 'a t) (acc : string) (to_string_method : ('a -> string)) = 
    match s_array with
    | h :: [] -> toString_helper [] (acc^(to_string_method h)^"]") to_string_method
    | h :: t -> toString_helper t (acc^(to_string_method h)^", ") to_string_method
    | [] -> acc
in
toString_helper (List.rev s_array) "[" to_string_method
  
end *)

let int_comparator x y = compare x y

let list_to_sort = [1; 4; 5; 2; 3]

let sorted_list = List_mergesort.sort list_to_sort int_comparator

let () = print_endline (List_mergesort.toString list_to_sort string_of_int)


(* let int_comparator (x : int) (y : int) : int = compare x y

let list_to_sort = [1; 4; 5; 2; 3]
let list_to_sort : int List_mergesort.t = (list_to_sort : int list :> int List_mergesort.t)

let sorted_list : int List_mergesort.t = List_mergesort.sort list_to_sort int_comparator *)