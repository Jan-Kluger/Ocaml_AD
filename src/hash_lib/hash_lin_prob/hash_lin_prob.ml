module HashLinearProbing : Hash_lib.Hash_sig.HASH_SIG with type ('a, 'b) t = (('a * 'b) option) array = struct
  type ('a, 'b) t = (('a * 'b) option) array

  let rec find_slot ~(hashtable: ('a, 'b) t) ~(key: 'a) ~(hash_function: 'a -> int) (start: int) (step: int) : int =
    let index = (start + step) mod Array.length hashtable in
    match hashtable.(index) with
    | None -> index
    | Some (k, _) when k = key -> index
    | _ -> find_slot ~hashtable ~key ~hash_function start (step + 1)

  let put ~(hashtable: ('a, 'b) t) ((key: 'a), (value: 'b)) ~(hash_function: 'a -> int) : ('a, 'b) t =
    let start = hash_function key mod Array.length hashtable in
    let index = find_slot ~hashtable ~key ~hash_function start 0 in
    hashtable.(index) <- Some (key, value);
    hashtable

  let get ~(hashtable: ('a, 'b) t) (key: 'a) ~(hash_function: 'a -> int) : 'b option =
    let start = hash_function key mod Array.length hashtable in
    let rec find_value step =
      let index = (start + step) mod Array.length hashtable in
      match hashtable.(index) with
      | None -> None
      | Some (k, v) when k = key -> Some v
      | _ -> find_value (step + 1)
    in
    find_value 0

  let remove ~(hashtable: ('a, 'b) t) (key: 'a) ~(hash_function: 'a -> int) : ('a, 'b) t =
    let start = hash_function key mod Array.length hashtable in
    let rec find_and_remove step =
      let index = (start + step) mod Array.length hashtable in
      match hashtable.(index) with
      | None -> hashtable
      | Some (k, _) when k = key ->
        hashtable.(index) <- None;
        hashtable
      | _ -> find_and_remove (step + 1)
    in
    find_and_remove 0

  let contains ~(hashtable: ('a, 'b) t) (key: 'a) ~(hash_function: 'a -> int) : bool =
    match get ~hashtable key ~hash_function with
    | Some _ -> true
    | None -> false

  let size (hashtable: ('a, 'b) t) : int =
    Array.fold_left (fun acc slot ->
      match slot with
      | Some _ -> acc + 1
      | None -> acc) 0 hashtable

  let clear (hashtable: ('a, 'b) t) : ('a, 'b) t =
    Array.map (fun _ -> None) hashtable
end
