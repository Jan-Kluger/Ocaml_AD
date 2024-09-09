module type SORT_SIG = sig
  type 'a t

  val sort : 'a t -> ('a -> 'a -> int) -> 'a t
  val toString : 'a t -> ('a -> string) -> string

end