module List_queue : Queue_sig.QUEUE_SIG with type 'a t = 'a list = struct

  type 'a t = 'a list

  let push_back (queue : 'a t) (element : 'a)  = queue @ [element]
  let pop_front (queue : 'a t) = 
    match queue with
      | [] -> None
      | h :: t -> Some (h,t)
  let front (queue : 'a t) =
    match queue with
      | [] -> None
      | h :: _ -> Some h
  let isEmpty (queue : 'a t) = 
    match queue with
    | [] -> true
    | _ -> false
  
    let toString (queue : 'a t) (to_string: 'a -> string) = 
    let rec string_helper (queue : 'a t) (acc : string) = 
      match queue with
      | [] -> acc
      | h::[] -> string_helper [] acc^to_string h^"]"
      | h::t -> string_helper t (acc ^ to_string h ^ ", ")
    in 
    string_helper (List.rev queue) "["
end