type 'a t = Nil | Cons of 'a Lazy.t * 'a t Lazy.t

let empty = Nil

let fail _ = Nil

let cons x xs = Cons (lazy x, lazy xs)

let pure a = Cons (lazy a, lazy Nil)

let rec map f = function
  | Nil -> Nil
  | Cons (x, xs) -> Cons (lazy (f (Lazy.force x)), lazy (map f (Lazy.force xs)))

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

let alt = concat

let rec fold_right f l i =
  match l with
  | Nil -> i
  | Cons (x, xs) -> f (Lazy.force x) (fold_right f (Lazy.force xs) i)

(* Create a singleton list from an already lazy value *)
let from_lazy l = Cons (l, lazy Nil)

(* Append a lazy value to the end of the lazy list. *)
let rec append_lazy xs x =
  match xs with
  | Nil -> from_lazy x
  | Cons (x, xs) -> Cons (x, lazy (append_lazy (Lazy.force xs) x))

(* Append a strict value on the lazy list. *)
let append xs x = append_lazy xs (Lazy.from_val x)

(* Force the evaluation of the spine of a list (its structure) but not the values within (that is, its Weak-Head Normal Form).
   This explicitely converts out of a Lazylist into a regular List, but with lazy values inside,
   to show that fact, and highlight that infinite lazylists cannot be evaluated even to WHNF. *)
let rec whnf = function Nil -> [] | Cons (x, xs) -> x :: whnf (Lazy.force xs)

(* Constructs a [Lazylist] from a list of lazy values. *)
let rec of_list_lazy = function
  | [] -> Nil
  | x :: xs -> Cons (x, lazy (of_list_lazy xs))

(* Construct a [Lazylist] from an already-evaluated [List]. *)
let of_list l = List.map Lazy.from_val l |> of_list_lazy

(* Forces evaluation of the entire list into a standard [List]. *)
let to_list l = whnf l |> List.map Lazy.force

let of_seq_lazy s = Seq.fold_left append_lazy empty s

let of_seq s = Seq.fold_left append empty s

(* Constructs a [Seq.t] from the given [Lazylist]. Evaluates only the spine of the list, but not the values. *)
let rec to_seq_lazy = function
  | Nil -> Seq.empty
  | Cons (x, xs) -> fun () -> Seq.Cons (x, to_seq_lazy (Lazy.force xs))

(* Constructs a [Seq.t] from the given [Lazylist]. Evaluates both the spine and the values of the list. *)
let to_seq l = to_seq_lazy l |> Seq.map Lazy.force

(* Returns the length of the list. Will hang on infinite lists. *)
let length l = whnf l |> List.length

(* Checks if the [Lazylist] is empty without evaluating anything about it. *)
let is_empty = function Nil -> true | Cons _ -> false

(* A lazy version of [fold_right] that allows short-circuiting for handling potentially infinite lists. *)
let rec foldr_lazy f l i =
  match l with Nil -> i | Cons (x, xs) -> f x (foldr_lazy f (Lazy.force xs) i)

let rec take n = function
  | Cons (x, xs) when n > 0 -> Cons (x, lazy (take (n - 1) (Lazy.force xs)))
  | _ -> Nil

let rec skip n = function
  | Cons (_, xs) when n > 0 -> skip (n - 1) (Lazy.force xs)
  | xs -> xs
