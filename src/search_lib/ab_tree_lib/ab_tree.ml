type 'a l_tree = 
| Nil 
| Node of 'a list * 'a l_tree list

(* This is the code for a 2 - 4 (a,b)-Tree *)

module Ab_Tree : Search_lib.Search_sig.SEARCH with type 'a t = 'a l_tree = struct
  type 'a t = 'a l_tree

  let rec locate (tree : 'a t) (element : 'a) : 'a option =
    match tree with
    | Nil -> None
    | Node (values, children) ->
        locate_in_node values children element

  and locate_in_node (values : 'a list) (children : 'a t list) (element : 'a) : 'a option =
    match values, children with
    | [], [] -> None
    | value :: _, _ when element = value -> Some value
    | value :: rest_values, child :: rest_children ->
        if element < value then
          locate child element
        else if rest_values = [] then
          locate (List.hd rest_children) element
        else
          locate_in_node rest_values rest_children element
    | _, _ -> None

  let rec insert (tree : 'a t) (element : 'a) : 'a t =
    match tree with
    | Nil -> Node ([element], [])
    | Node (values, children) ->
        if children = [] then
          let new_values = List.sort compare (element :: values) in
          if List.length new_values < 4 then
            Node (new_values, [])
          else
            split_node new_values []
        else
          let child_index = find_child_index values element in
          let new_child = insert (List.nth children child_index) element in
          let new_children = update_child children child_index new_child in
          if List.length new_children <= 4 then
            Node (values, new_children)
          else
            split_internal_node values new_children

  and find_child_index (values : 'a list) (element : 'a) : int =
    let rec aux idx = function
      | [] -> idx
      | value :: _ when element < value -> idx
      | _ :: rest -> aux (idx + 1) rest
    in
    aux 0 values

  and update_child (children : 'a t list) (index : int) (new_child : 'a t) : 'a t list =
    List.mapi (fun i child -> if i = index then new_child else child) children

  and split_list (lst : 'a list) (middle_index : int) : 'a list * 'a * 'a list =
    let rec aux idx left mid right = function
      | [] -> (List.rev left, mid, List.rev right)
      | x :: xs ->
          if idx < middle_index then aux (idx + 1) (x :: left) mid right xs
          else if idx = middle_index then aux (idx + 1) left x right xs
          else aux (idx + 1) left mid (x :: right) xs
    in
    aux 0 [] (List.hd lst) [] lst

  and split_node (values : 'a list) (children : 'a t list) : 'a t =
    let middle_index = (List.length values) / 2 in
    let left_values, middle_value, right_values = split_list values middle_index in
    let left_node = Node (left_values, firstn middle_index children) in
    let right_node = Node (right_values, skipn middle_index children) in
    Node ([middle_value], [left_node; right_node])

  and firstn n lst =
    let rec aux i acc = function
      | [] -> List.rev acc
      | x :: xs -> if i < n then aux (i + 1) (x :: acc) xs else List.rev acc
    in
    aux 0 [] lst

  and skipn n lst =
    let rec aux i = function
      | [] -> []
      | x :: xs -> if i < n then aux (i + 1) xs else x :: xs
    in
    aux 0 lst

  and split_internal_node (values : 'a list) (children : 'a t list) : 'a t =
    let middle_index = (List.length values) / 2 in
    let left_values, middle_value, right_values = split_list values middle_index in
    let left_children = firstn (middle_index + 1) children in
    let right_children = skipn (middle_index + 1) children in
    let left_node = Node (left_values, left_children) in
    let right_node = Node (right_values, right_children) in
    Node ([middle_value], [left_node; right_node])

    let rec remove (tree : 'a t) (element : 'a) : 'a t =
      match tree with
      | Nil -> Nil
      | Node (values, children) ->
          if List.exists ((=) element) values then
            remove_from_node (Node (values, children)) element
          else
            let new_children = remove_from_children values children element in
            if List.length values = 0 && List.length new_children = 1 then
              List.hd new_children (* Collapse the tree if the root becomes empty *)
            else
              Node (values, new_children)
    
    and remove_from_node (node : 'a t) (element : 'a) : 'a t =
      match node with
      | Nil -> Nil
      | Node (values, children) ->
          if children = [] then
            let new_values = List.filter ((<>) element) values in
            Node (new_values, [])
          else
            handle_internal_remove node element
    
    and handle_internal_remove (node : 'a t) (element : 'a) : 'a t =
      match node with
      | Nil -> Nil
      | Node (values, children) ->
          let rec find_and_remove idx = function
            | [] -> failwith "Element not found"
            | value :: rest when value = element ->
                let left_child = List.nth children idx in
                let right_child = List.nth children (idx + 1) in
                (match left_child, right_child with
                 | Node (l_vals, _), Node (r_vals, _) ->
                     if List.length l_vals > 1 then
                       let largest_in_left = List.hd (List.rev l_vals) in
                       let new_left = remove left_child largest_in_left in
                       Node (largest_in_left :: rest, new_left :: List.tl children)
                     else if List.length r_vals > 1 then
                       let smallest_in_right = List.hd r_vals in
                       let new_right = remove right_child smallest_in_right in
                       Node (smallest_in_right :: rest, left_child :: new_right :: List.tl (List.tl children))
                     else
                       let merged = merge_nodes left_child value right_child in
                       Node (rest, List.mapi (fun i c -> if i = idx then merged else c) children)
                 | _ -> failwith "Invalid node structure")
            | _ :: rest -> find_and_remove (idx + 1) rest
          in
          find_and_remove 0 values
    
    and remove_from_children (values : 'a list) (children : 'a t list) (element : 'a) : 'a t list =
      let rec aux vals chs elt =
        match vals, chs with
        | [], child :: rest -> remove child elt :: rest
        | v :: vs, child :: rest ->
            if elt < v then remove child elt :: rest
            else child :: aux vs rest elt
        | _, _ -> children
      in
      aux values children element
    
    and merge_nodes (left : 'a t) (parent_value : 'a) (right : 'a t) : 'a t =
      match left, right with
      | Node (left_values, left_children), Node (right_values, right_children) ->
          Node (left_values @ [parent_value] @ right_values, left_children @ right_children)
      | _ -> failwith "Invalid merge"
    let rec toString (tree : 'a t) (to_string : ('a -> string)) : string =
      match tree with
      | Nil -> "Nil"
      | Node (values, children) ->
          let values_str = String.concat ", " (List.map to_string values) in
          let children_str = List.map (fun child -> toString child to_string) children in
          let children_str_concat = String.concat "\n" (List.map (fun s -> "  " ^ s) children_str) in
          Printf.sprintf "Node(%s)\n%s" values_str children_str_concat  
end