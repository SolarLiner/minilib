module Make : functor (I : Minicat.Integer.INTEGER) -> sig
  include Minicat.Integer.INTEGER

  val of_string : string -> t

  val to_string : t -> string

  val div : t -> t -> t

  val ( / ) : t -> t -> t

  val make : I.t -> I.t -> t

  val parts : t -> I.t * I.t
end
