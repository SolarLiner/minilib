open Minicat

module Inner = struct
  type stream = char Lazylist.t

  type offset = int

  type 'a reply =
    | Error of (string * offset)
    | Match of ('a * offset)
    | Parse of 'a t * stream

  and 'a t = P of (stream -> 'a reply)

  let rec parse (P p) input =
    match p input with
    | Error err -> Result.Error err
    | Parse (cont, input') -> parse cont input'
    | Match (x, _) -> Result.Ok x

  let fail s = P (fun _ -> Error (s, 0))

  let empty = fail "Empty parse"

  let pure a = P (fun _ -> Match (a, 0))

  let rec map f (P p) =
    let mapinner = function
      | Error _ as err -> err
      | Match (x, off) -> Match (f x, off)
      | Parse (cont, stream) -> Parse (map f cont, stream)
    in
    P (fun input -> p input |> mapinner)

  let rec app (P f) (P x) =
    let inner input =
      match f input with
      | Error err -> Error err
      | Parse (cont, input') -> Parse (app cont (P x), input')
      | Match (f, off) -> (
          let rest = Lazylist.skip off input in
          match x rest with
          | Error err -> Error err
          | Parse (cont, rest') -> Parse (map f cont, rest')
          | Match (x, off') -> Match (f x, off + off'))
    in
    P inner

  let rec bind (P m) f =
    P
      (fun input ->
        match m input with
        | Error err -> Error err
        | Match (x, off) -> (
            let (P p) = f x in
            match p (Lazylist.skip off input) with
            | Match (x', off') -> Match (x', off + off')
            | Error (err, off') -> Error (err, off + off')
            | x -> x)
        | Parse (cont, stream) -> Parse (bind cont f, stream))

  (* let alt (P a) (P b) =
     let inner input =
       let c r = if Lazylist.is_empty r then b input else r in
       c (a input)
     in
     P inner *)
  let alt (P a) (P b) =
    P
      (fun input ->
        match a input with
        | Error (err, off) ->
            Printf.eprintf "Skipping error %S at offset %d\n" err off;
            b input
        | x -> x)
end

include Monad.Make (Inner)
include Alternative.Make (Inner)

let parse = Inner.parse

let parse_string p s = parse p (String.to_seq s |> Lazylist.of_seq)

let parse_file p filepath =
  let stream =
    Lazylist.unfold_right
      (fun c ->
        try
          let s = input_line c in
          Some (Lazylist.of_seq (String.to_seq s), c)
        with End_of_file -> None)
      (open_in filepath)
    |> Lazylist.join
  in
  parse p stream

let fail s = Inner.P (fun _ -> Inner.Error (s, 0))

let opt p = Option.some <$> p <|> pure None

let fix' f =
  let rec p = lazy (f r) and r i = (Lazy.force p) i in
  r

let fix f = Inner.P (fix' (fun f' input -> Inner.Parse (f (Inner.P f'), input)))

let eof =
  Inner.P
    (fun input ->
      if Lazylist.is_empty input then Match ((), 0)
      else Error ("Expected EOF", 0))

let any =
  Inner.P
    (fun input ->
      match Lazylist.first input with
      | Some c ->
          Printf.eprintf "trace: any: %C\n" c;
          Parse (return c, Lazylist.skip 1 input)
      | None -> Error ("Unexpected EOF", 1))

let rec advance = function
  | n when n <= 0 -> pure ()
  | n -> any *> advance (n - 1)

let rec span (Inner.P p) =
  let inner input =
    match p input with
    | Error err -> Inner.Error err
    | Match (x, off) -> Inner.Match ((x, Lazylist.take off input), off)
    | Parse (p, input') -> Inner.Parse (span p, input')
  in
  Inner.P inner

let ( --> ) pred p =
  let> x, s = span p in
  if pred x then pure x
  else
    fail (Printf.sprintf "Unexpected %S" (Lazylist.to_seq s |> String.of_seq))

let label s (Inner.P p) =
  Inner.P
    (fun input ->
      match p input with
      | Error (err, off) -> Error (s ^ ": " ^ err, off)
      | x -> x)

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
      let> c = any in
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

module Sexp = struct
  type ast = Atom of string | Call of string * ast list

  let ident =
    let of_char cl = List.to_seq cl |> String.of_seq in
    of_char <$> many1 alphanum

  let parens p = surround (char '(') (char ')' |> token) p

  let atom =
    (let$ s = token ident in
     Atom s)
    <|> fail "Expected atom"

  (* let sexp =
     fix' (fun sexp atom ->
         parens
           (let mk n d = Call (n, d) in
            mk <$> token ident <*> many (sexp atom))
         <|> atom) *)
  let sexp =
    fix (fun sexp ->
        parens
          (let> name = token ident in
           let$ args = many sexp in
           Call (name, args))
        <|> atom)
    |> token
end
