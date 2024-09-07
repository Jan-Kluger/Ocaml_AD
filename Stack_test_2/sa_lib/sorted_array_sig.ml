module type SORTED_ARRAY_SIG = sig
  type 'a t

  val insert : 'a t -> 'a -> ('a -> 'a -> int) -> 'a t
  val remove : 'a t -> 'a -> 'a t
  val find : 'a t -> 'a -> ('a -> 'a -> int) -> 'a option
  val toString : 'a t -> ('a -> string) -> string
end