open Minicat

include Monad.MONAD

include Comonad.COMONAD with type 'a t := 'a t

include Alternative.ALTERNATIVE with type 'a t := 'a t

include Cons.CONS with type 'a t := 'a t

include Foldable.FOLDABLE with type 'a t := 'a t

include Unfoldable.UNFOLDABLE with type 'a t := 'a t

val first : 'a t -> 'a option

val from_lazy : 'a lazy_t -> 'a t

val append_lazy : 'a t -> 'a lazy_t -> 'a t

val append : 'a t -> 'a -> 'a t

val whnf : 'a t -> 'a lazy_t list

val of_list_lazy : 'a lazy_t list -> 'a t

val of_list : 'a list -> 'a t

val to_list : 'a t -> 'a list

val of_seq_lazy : 'a lazy_t Seq.t -> 'a t

val of_seq : 'a Seq.t -> 'a t

val to_seq_lazy : 'a t -> 'a lazy_t Seq.t

val to_seq : 'a t -> 'a Seq.t

val length : 'a t -> int

val is_empty : 'a t -> bool

val foldr_lazy : ('a lazy_t -> 'b -> 'b) -> 'a t -> 'b -> 'b

val next : 'a t -> ('a * 'a t) option

val take : int -> 'a t -> 'a t

val skip : int -> 'a t -> 'a t
