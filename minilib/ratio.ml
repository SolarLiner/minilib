open Minicat

module Make (I : Integer.INTEGER) : sig
  include Integer.INTEGER

  include Ord.ORD with type t := t

  val of_string : string -> t

  val to_string : t -> string

  val div : t -> t -> t

  val ( / ) : t -> t -> t

  val make : I.t -> I.t -> t

  val parts : t -> I.t * I.t
end = struct
  module I = Integer.Make (I)

  type t = R of (I.t * I.t)

  let equal (R (n, d)) (R (n', d')) = I.equal n n' && I.equal d d'

  let compare _ _ = failwith "Ratio: todo"

  let of_string s =
    match String.split_on_char '/' s with
    | [ n ] -> R (I.of_string n, I.one)
    | [ n; d ] -> R (I.of_string n, I.of_string d)
    | _ -> failwith "Ratio.of_string: incorrect format"

  let to_string (R (n, d)) =
    Printf.sprintf "%s/%s" (I.to_string n) (I.to_string d)

  let normalize (R (n, d)) =
    let g = I.gcd n d in
    let s = I.mul (I.signum n) (I.signum d) in
    let open I in
    R (s * I.div n g, I.div d g)

  let add (R (n1, d1)) (R (n2, d2)) =
    let open I in
    R ((n1 * d2) + (n2 * d1), d1 * d2) |> normalize

  let sub (R (n1, d1)) (R (n2, d2)) =
    let open I in
    R ((n1 * d2) - (n2 * d1), d1 * d2) |> normalize

  let mul (R (n1, d1)) (R (n2, d2)) =
    let open I in
    R (n1 * n2, d1 * d2) |> normalize

  let abs (R (n, d)) = R (I.abs n, d)

  let quotrem x (R (n, d)) =
    let (R (n, d)) = mul x (R (d, n)) in
    let x, y = I.quotrem n d in
    (R (x, I.one), R (y, I.one))

  let signum (R (n, _)) = R (I.signum n, I.one)

  let div a (R (a', b')) = mul a (R (b', a'))

  let ( / ) = div

  let make n d = R (n, d)

  let parts (R (n, d)) = (n, d)
end
