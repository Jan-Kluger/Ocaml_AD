module type HASH_SIG = sig
    (* The type of the hash table storing values of type 'b *)
    type ('a, 'b) t  
    
    (* Insert or update a key-value pair in the hash table *)
    val put : hashtable:(('a, 'b) t) -> ('a * 'b) -> hash_function:('a -> int) -> (('a, 'b) t)
  
    (* Retrieve the value associated with the key *)
    val get : hashtable:(('a, 'b) t) -> ('a * 'b) -> hash_function:('a -> int) -> 'b option
  
    (* Remove a key-value pair from the hash table *)
    val remove : hashtable:(('a, 'b) t) -> 'a -> hash_function:('a -> int) -> (('a, 'b) t)
  
    (* Check if a key exists in the hash table *)
    val contains : hashtable:(('a, 'b) t) -> 'a -> hash_function:('a -> int) -> bool
  
    (* Return the number of key-value pairs in the hash table *)
    val size : (('a, 'b) t) -> int
  
    (* Clear the entire hash table *)
    val clear : (('a, 'b) t) -> (('a, 'b) t)
  end
  