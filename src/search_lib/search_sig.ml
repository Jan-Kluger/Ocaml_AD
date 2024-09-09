module type SEARCH = sig
  type 'a t

  val insert : 'a t -> 'a -> 'a t
  val remove : 'a t -> 'a -> 'a t
  val locate : 'a t -> 'a -> 'a option
  val toString : 'a t -> ('a -> string) -> string
end