module type QUEUE_SIG = sig
    type 'a t
    
    val push_back : 'a t -> 'a -> 'a t
    val pop_front : 'a t -> ('a * 'a t) option
    val front : 'a t -> 'a option
    val isEmpty : 'a t -> bool
    val toString : 'a t -> ('a -> string) -> string
  end