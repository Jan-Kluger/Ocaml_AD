module ChainHash : Hash_lib.Hash_sig.HASH_SIG with type ('a, 'b) t = ('a * 'b) list array = struct
  type ('a, 'b) t = ('a * 'b) list array

  let put ~(hashtable: ('a, 'b) t) ((key: 'a), (value: 'b)) ~(hash_function: 'a -> int) : ('a, 'b) t =
    let index = hash_function key mod Array.length hashtable in
    hashtable.(index) <- (key, value) :: hashtable.(index);
    hashtable

  let get ~(hashtable: ('a, 'b) t) ((key: 'a), _) ~(hash_function: 'a -> int) : 'b option =
    let index = hash_function key mod Array.length hashtable in
    let rec find_in_list = function
      | [] -> None
      | (k, v) :: rest -> if k = key then Some v else find_in_list rest
    in
    find_in_list hashtable.(index)

  let remove ~(hashtable: ('a, 'b) t) (key: 'a) ~(hash_function: 'a -> int) : ('a, 'b) t =
    let index = hash_function key mod Array.length hashtable in
    hashtable.(index) <- List.filter (fun (k, _) -> k <> key) hashtable.(index);
    hashtable

  let contains ~(hashtable: ('a, 'b) t) (key: 'a) ~(hash_function: 'a -> int) : bool =
    match get ~hashtable (key, Obj.magic ()) ~hash_function with
    | Some _ -> true
    | None -> false

  let size (hashtable: ('a, 'b) t) : int =
    Array.fold_left (fun acc bucket -> acc + List.length bucket) 0 hashtable

  let clear (hashtable: ('a, 'b) t) : ('a, 'b) t =
    Array.map (fun _ -> []) hashtable
end
