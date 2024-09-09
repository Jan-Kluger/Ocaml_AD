module type STACK_SIG = sig
  type 'a t
  
  val push : 'a t -> 'a -> 'a t
  val pop : 'a t -> ('a * 'a t)
  val peek : 'a t -> 'a option
  val isEmpty : 'a t -> bool
  val toString : 'a t -> ('a -> string) -> string
end