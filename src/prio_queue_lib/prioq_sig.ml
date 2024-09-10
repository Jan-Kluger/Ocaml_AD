module type PRIORITY_QUEUE = sig
  type 'a t

  val insert : 'a t -> 'a -> comp:('a -> 'a -> bool) -> 'a t
  val build : 'a list -> ('a -> 'a -> bool)  -> 'a t
  val min : 'a t -> 'a option
  val delete_min : 'a t -> ('a -> 'a -> bool) -> 'a t option
  val to_string : 'a t -> ('a -> string) -> string

end