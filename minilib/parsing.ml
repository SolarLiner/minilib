open Minicat

module Inner = struct
  type stream = char Lazylist.t

  type offset = int

  type 'a reply =
    | Error of (string * offset)
    | Match of ('a * offset)
    | Parse of 'a t * stream

  and 'a t = stream -> 'a reply

  let rec parse p input =
    match p input with
    | Error err -> Result.Error err
    | Parse (cont, input') -> parse cont input'
    | Match (x, _) -> Result.Ok x

  let fail s _ = Error (s, 0)

  let empty = fail "Empty parse"

  let pure a _ = Match (a, 0)

  let rec map f p =
    let mapinner = function
      | Error _ as err -> err
      | Match (x, off) -> Match (f x, off)
      | Parse (cont, stream) -> Parse (map f cont, stream)
    in
    fun input -> p input |> mapinner

  let rec app f x input =
    match f input with
    | Error err -> Error err
    | Parse (cont, input') -> Parse (app cont x, input')
    | Match (f, off) -> (
        let rest = Lazylist.skip off input in
        match x rest with
        | Error err -> Error err
        | Parse (cont, rest') -> Parse (map f cont, rest')
        | Match (x, off') -> Match (f x, off + off'))

  let rec bind m f input =
    match m input with
    | Error err -> Error err
    | Match (x, off) -> (
        match (f x) (Lazylist.skip off input) with
        | Match (x', off') -> Match (x', off + off')
        | Error (err, off') -> Error (err, off + off')
        | x -> x)
    | Parse (cont, stream) -> Parse (bind cont f, stream)

  (* let alt (P a) (P b) =
     let inner input =
       let c r = if Lazylist.is_empty r then b input else r in
       c (a input)
     in
     P inner *)
  let alt a b input =
    match a input with
    | Error (err, off) ->
        Printf.eprintf "Skipping error %S at offset %d\n" err off;
        b input
    | x -> x
end

include Inner

open Monad.Make (Inner)

open Alternative.Make (Inner)

let parse = Inner.parse

let parse_string p s = parse p (String.to_seq s |> Lazylist.of_seq)

let parse_file p filepath =
  let stream =
    let open Monad.Make (Lazylist) in
    Lazylist.unfold_right
      (fun c ->
        try
          let s = input_line c in
          Some (Lazylist.of_seq (String.to_seq s), c)
        with End_of_file -> None)
      (open_in filepath)
    |> join
  in
  parse p stream

(* let fix' f =
     let rec p = lazy (f r) and r i = (Lazy.force p) i in
     r

   let fix f =
     fix' (fun f' input ->
         match f input with Inner.Parse (cont, input') -> f f' input' | x -> x) *)

let fix f input =
  match f empty input with Parse (f', input') -> f f' input' | x -> x

let eof input =
  if Lazylist.is_empty input then Inner.Match ((), 0)
  else Error ("Expected EOF", 0)

let any input =
  match Lazylist.next input with
  | Some (c, input') ->
      Printf.eprintf "trace: any: %C\n" c;
      Inner.Parse (return c, input')
  | None -> Error ("Unexpected EOF", 1)

let peek input =
  match Lazylist.first input with
  | None -> Error ("Unexpected EOF", 1)
  | Some x -> Parse (return x, input)

let rec advance = function
  | n when n <= 0 -> pure ()
  | n -> any *> advance (n - 1)

let rec span p input =
  match p input with
  | Inner.Error err -> Inner.Error err
  | Inner.Match (x, off) ->
      Inner.Match
        ((x, Lazylist.take off input |> Lazylist.to_seq |> String.of_seq), off)
  | Inner.Parse (p, input') -> Inner.Parse (span p, input')

let ( --> ) pred p =
  let* x, s = span p in
  if pred x then pure x else fail (Printf.sprintf "Unexpected %S" s)

let label s p input =
  match p input with
  | Inner.Error (err, off) -> Inner.Error (s ^ ": " ^ err, off)
  | x -> x

let ( <?> ) p s = label s p

module WithOrd (O : Ord.ORD) = struct
  open Ord.Make (O)

  let elem el p = ( == ) el --> p
end

include struct
  open WithOrd (Ord.StructOrd (Char))

  let char c = elem c any <|> fail (Printf.sprintf "Unexpected char %C" c)

  let keyword s =
    label ("keyword " ^ s)
      (String.to_seq s |> Seq.map char |> List.of_seq |> sequence
     |> map List.to_seq |> map String.of_seq)

  let loweralpha =
    let pred = function 'a' .. 'z' -> true | _ -> false in
    pred --> any

  let upperalpha =
    let pred = function 'A' .. 'Z' -> true | _ -> false in
    pred --> any

  let alpha = loweralpha <|> upperalpha <?> "alpha"

  let digit =
    let pred = function '0' .. '9' -> true | _ -> false in
    let inner =
      let* c = any in
      Printf.eprintf "trace: digit: got %c\n" c;
      return c
    in
    pred --> inner <?> "digit"

  let alphanum = alpha <|> digit <?> "alphanumeric"

  let space =
    let pred = function ' ' | '\t' | '\n' -> true | _ -> false in
    pred --> any

  let token p = many space *> p
end

let surround left right p = left *> p <* right
