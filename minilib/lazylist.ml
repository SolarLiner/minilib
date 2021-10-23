open Minicat

module Inner = struct
  type 'a t = Nil | Cons of 'a Lazy.t * 'a t Lazy.t

  let empty = Nil

  let cons x xs = Cons (lazy x, lazy xs)

  let pure a = Cons (lazy a, lazy Nil)

  let rec map f = function
    | Nil -> Nil
    | Cons (x, xs) ->
        Cons (lazy (f (Lazy.force x)), lazy (map f (Lazy.force xs)))

  let rec app fs xs =
    match (fs, xs) with
    | Cons (f, fs), Cons (x, xs) ->
        Cons
          ( lazy ((Lazy.force f) (Lazy.force x)),
            lazy (app (Lazy.force fs) (Lazy.force xs)) )
    | _ -> Nil

  let rec concat xs ys =
    match xs with
    | Nil -> ys
    | Cons (x, xs) -> Cons (x, lazy (concat (Lazy.force xs) ys))

  let rec bind m f =
    match m with
    | Nil -> Nil
    | Cons (x, xs) ->
        let xs' = f (Lazy.force x) in
        concat xs' (bind (Lazy.force xs) f)
end

include Cons.Make (Inner)
include Monad.Make (Inner)

(* Force the evaluation of the spine of a list (its structure) but not the values within (that is, its Weak-Head Normal Form).
   This explicitely converts out of a Lazylist into a regular List, but with lazy values inside,
   to show that fact, and highlight that infinite lazylists cannot be evaluated even to WHNF. *)
let rec whnf = function
  | Inner.Nil -> []
  | Inner.Cons (x, xs) -> x :: whnf (Lazy.force xs)
