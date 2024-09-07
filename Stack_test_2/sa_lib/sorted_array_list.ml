module Sorted_array_list : Sorted_array_sig.SORTED_ARRAY_SIG with type 'a t = 'a list = struct
  type 'a t = 'a list


  (* let insert (s_array : 'a t) (element : 'a) (comparator : 'a -> 'a -> int) : 'a t =
    let rec insert_helper s_array element comparator =
      match s_array with
      | h :: t -> 
          if comparator element h <= 0 then
            element :: s_array
          else
            h :: insert_helper t element comparator  
      | [] -> [element]
    in
    insert_helper s_array element comparator *)

  (* let split_array (arr : 'a t) (index : int) : ('a t * 'a t)= 
    let rec split_array_helper (arr : 'a t) (index : int) (acc : 'a t) = 
      match index with
      | x when x <= 0 -> ( acc, arr)
      | i -> match arr with
      | h :: t -> split_array_helper t (i-1) (h :: acc)
      | [] -> ((arr, []))
    in
      match arr with
      | [] -> ([], [])
      | h :: [] -> ([h], [])
      | _ :: _ -> split_array_helper arr index []

  let insert (s_array : 'a t) (element : 'a) (comparator : 'a -> 'a -> int) : 'a t =
    let rec find_index (s_array : 'a t) (element : 'a) (comparator : 'a -> 'a -> int) (lower : int) (upper : int) =
      if lower >= upper then
        lower
      else
        let mid = (lower + upper) / 2 in
        match comparator (List.nth s_array mid) element with
        | 0 -> mid
        | x when x < 0 -> find_index s_array element comparator 0 (mid - 1)
        | _ -> find_index s_array element comparator (mid + 1) upper
    in
    match split_array s_array (find_index s_array element comparator 0 (List.length s_array)) with
    | (l, r) -> l @ [element] @ r *)

    (* Helper function to split the array at the given index *)
let split_array (arr : 'a t) (index : int) : ('a t * 'a t) = 
  let rec split_array_helper arr index acc = 
    match index, arr with
    | 0, _ -> (List.rev acc, arr)
    | _, [] -> (List.rev acc, [])
    | i, h :: t -> split_array_helper t (i - 1) (h :: acc)
  in
  split_array_helper arr index []

(* Find the correct index to insert the element using binary search *)
let rec find_index (s_array : 'a t) (element : 'a) (comparator : 'a -> 'a -> int) (lower : int) (upper : int) =
  if lower >= upper then lower
  else
    let mid = (lower + upper) / 2 in
    match comparator (List.nth s_array mid) element with
    | 0 -> mid
    | x when x < 0 -> find_index s_array element comparator lower mid
    | _ -> find_index s_array element comparator (mid + 1) upper

(* Insert element into the sorted array *)
let insert (s_array : 'a t) (element : 'a) (comparator : 'a -> 'a -> int) : 'a t =
  let index = find_index s_array element comparator 0 (List.length s_array) in
  let (l, r) = split_array s_array index in
  l @ (element :: r)

  let remove (s_array : 'a t) (element : 'a) : 'a t =
    let rec remove_helper (s_array : 'a t) (element : 'a) (acc : 'a t) =
      match s_array with
      | h::t -> if h == element then
                  acc @ t
                else
                  remove_helper t element acc
      | [] -> acc
    in
    remove_helper s_array element []

  (* let rec find (s_array : 'a t) (element : 'a) : 'a option =
    match s_array with
    | h :: t -> if h == element then
                  Some h
                else
                  find t element
    | [] -> None *)


    let find (s_array : 'a t) (element : 'a) (comparator : 'a -> 'a -> int) : 'a option =
      let rec find_helper (s_array : 'a t) (lower : int) (upper : int) (element : 'a) (comparator : 'a -> 'a -> int) =
        if lower > upper then
          None
        else
          let mid = (lower + upper)/2 in
          let toCompare = List.nth s_array mid in
          match comparator element toCompare with
          | 0 -> Some toCompare
          | x when x < 0 -> find_helper s_array lower (mid - 1) element comparator
          | _ -> find_helper s_array (mid + 1) upper element comparator 
        in
      find_helper s_array 0 (List.length s_array) element comparator

  let toString (s_array : 'a t) (to_string_method : ('a -> string)) = 
    let rec toString_helper (s_array : 'a t) (acc : string) (to_string_method : ('a -> string)) = 
      match s_array with
      | h :: [] -> toString_helper [] (acc^(to_string_method h)^"]") to_string_method
      | h :: t -> toString_helper t (acc^(to_string_method h)^", ") to_string_method
      | [] -> acc
  in
  toString_helper (List.rev s_array) "[" to_string_method
end