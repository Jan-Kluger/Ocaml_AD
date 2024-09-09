module List_stack : Stack_sig.STACK_SIG with type 'a t = 'a list = struct

  type 'a t = 'a list


  let pop (stack : 'a t) = 
    match stack with
    | [] -> failwith "empty stack"
    | h :: t -> (h,t)

  let push (stack : 'a t) (element : 'a) = element :: stack
  
  let peek (stack : 'a t ) = 
    match stack with
    | [] -> None
    | h :: _ -> Some h

  let isEmpty (stack : 'a t) = 
    match stack with
    | [] -> true
    | _ :: _ -> false

  let toString (stack : 'a t) (to_string: 'a -> string) = 
    let rec string_helper (stack : 'a t) (acc : string) = 
      match stack with
      | [] -> acc
      | h::[] -> string_helper [] acc^to_string h^"]"
      | h::t -> string_helper t (acc ^ to_string h ^ ", ")
    in 
    string_helper (List.rev stack) "["
end
