open Minicat

module Inner = struct
  type stream = char Lazylist.t

  type offset = int

  type 'a reply =
    | Error of (string * offset)
    | Match of ('a * offset)
    | Parse of (offset * 'a t * stream)

  and 'a t = stream -> 'a reply

  let update_offset off = function
    | Error (err, off') -> Error (err, off + off')
    | Match (x, off') -> Match (x, off + off')
    | Parse (off', cont, input') -> Parse (off + off', cont, input')

  let get_result = function
    | Error (err, off) -> Result.Error (err, off)
    | Match (x, _) -> Result.Ok x
    | Parse _ -> assert false

  let rec parse_raw p input =
    match p input with
    | Parse (off, cont, input') -> parse_raw cont input' |> update_offset off
    | x -> x

  let parse p input = parse_raw p input |> get_result

  let fail s _ = Error (s, 0)

  let empty = fail "Empty parse"

  let pure a _ = Match (a, 0)

  let[@inline] rec map f p input =
    match p input with
    | Error _ as err -> err
    | Match (x, off) -> Match (f x, off)
    | Parse (off, cont, stream) -> Parse (off, map f cont, stream)

  let rec app f x input =
    match f input with
    | Error _ as err -> err
    | Parse (off, cont, input') -> Parse (off, app cont x, input')
    | Match (f, off) -> Parse (off, map f x, Lazylist.skip off input)

  (* FIXME: Rigorous updating of offsets (with parse_raw ?) + Make [Parse] also include offset details *)
  let[@inline] rec bind m f input =
    match m input with
    | Error _ as err -> err
    | Match (x, off) -> Parse (off, f x, Lazylist.skip off input)
    | Parse (off, cont, input') -> Parse (off, bind cont f, input')

  (* let alt (P a) (P b) =
     let inner input =
       let c r = if Lazylist.is_empty r then b input else r in
       c (a input)
     in
     P inner *)
  let[@inline] alt a b input =
    match parse_raw a input with
    | Match _ as m -> m
    | Error (err, _) -> (
        match b input with
        | Error (err', off) ->
            Error (Printf.sprintf "Errors: %s and %s" err err', off)
        | x -> x)
    | _ -> assert false
end

include Inner

open Monad.Make (Inner)

open Alternative.Make (Inner)

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

let rec fix f input = f (fix f) input

let eof input =
  if Lazylist.is_empty input then Inner.Match ((), 0)
  else Error ("Expected EOF", 0)

let any input =
  match Lazylist.first input with
  | Some c -> Match (c, 1)
  | None -> Error ("Unexpected EOF", 1)

let peek input =
  match Lazylist.first input with
  | None -> Error ("Unexpected EOF", 1)
  | Some x -> Match (x, 0)

let advance n _ = Match ((), n)

let read n input =
  let value = Lazylist.take n input |> Lazylist.to_seq |> String.of_seq in
  if String.length value = n then Match (value, n)
  else Error (Printf.sprintf "read(%d): Unexpected EOF" n, String.length value)

let rec span p input =
  match p input with
  | Inner.Error err -> Inner.Error err
  | Inner.Match (x, off) ->
      Inner.Match
        ((x, Lazylist.take off input |> Lazylist.to_seq |> String.of_seq), off)
  | Inner.Parse (off, p, input') -> Inner.Parse (off, span p, input')

let where pred p =
  let* x, s = span p in
  if pred x then pure x else fail (Printf.sprintf "Unexpected %S" s)

let label s p input =
  match p input with
  | Inner.Error (err, off) -> Inner.Error (s ^ ": " ^ err, off)
  | x -> x

let ( <?> ) p s = label s p

module WithOrd (O : Ord.ORD) = struct
  open Ord.Make (O)

  let elem el p = where (( == ) el) p
end

include struct
  open WithOrd (Ord.StructOrd (Char))

  let char c = elem c any <?> Printf.sprintf "char %C" c

  let keyword s =
    let* v = read (String.length s) in
    guard ~message:(Printf.sprintf "Expected keyword %S" s) (pure (s = v))
    >> pure v

  let loweralpha =
    let pred = function 'a' .. 'z' -> true | _ -> false in
    where pred any

  let upperalpha =
    let pred = function 'A' .. 'Z' -> true | _ -> false in
    where pred any

  let alpha = loweralpha <|> upperalpha <?> "alpha"

  let digit =
    let pred = function '0' .. '9' -> true | _ -> false in
    let inner =
      let+ c = any in
      c
    in
    where pred inner <?> "digit"

  let alphanum = alpha <|> digit <?> "alphanumeric"

  let space =
    let pred = function ' ' | '\t' | '\n' -> true | _ -> false in
    where pred any

  let token p = many space *> p
end

let surround left right p = left *> p <* right
