open Minicat

module Inner = struct
  type sign = P | N

  type t = BigInt of sign * Bytes.t

  let bsplit b = (Bytes.get b 0, Bytes.sub b 1 (Bytes.length b - 1))

  let bsplitb b =
    let l = Bytes.length b - 1 in
    (Bytes.sub b 0 l, Bytes.get b l)

  let app i b = Bytes.concat Bytes.empty [ b; Bytes.make 1 (char_of_int i) ]

  let prep i b = Bytes.concat Bytes.empty [ Bytes.make 1 (char_of_int i); b ]

  let revsign = function P -> N | N -> P

  let is_zero b = Bytes.length b = 1 && Bytes.get b 0 = char_of_int 0

  let btos b =
    Bytes.map (fun c -> int_of_char c + int_of_char '0' |> char_of_int) b
    |> Bytes.to_string

  let stob s =
    String.to_bytes s
    |> Bytes.map (fun c -> int_of_char c - int_of_char '0' |> char_of_int)
  (* |> normalize *)

  let rec btrim c b =
    if Bytes.length b = 0 then b
    else
      let b', b'' = bsplit b in
      if b' = c then btrim c b'' else b

  let normalize b =
    let n = btrim (char_of_int 0) b in
    if Bytes.length n = 0 then Bytes.make 1 (char_of_int 0) else n

  let of_string s =
    if String.get s 0 = '-' then
      let s' = String.sub s 1 (String.length s - 1) in
      BigInt (N, stob s')
    else if String.get s 0 = '+' then
      let s' = String.sub s 1 (String.length s - 1) in
      BigInt (P, stob s')
    else BigInt (P, stob s)

  let to_string (BigInt (sign, b)) =
    let prefix = match sign with P -> "" | N -> "-" in
    prefix ^ btos b

  let equal (BigInt (p1, x)) (BigInt (p2, y)) = p1 = p2 && Bytes.equal x y

  let compare (BigInt (p1, x)) (BigInt (p2, y)) =
    let rec cmpb flip a b =
      if is_zero a then if flip then Ord.GT else Ord.LT
      else if is_zero b then if flip then Ord.LT else Ord.GT
      else
        let a', a'' = bsplitb a in
        let b', b'' = bsplitb b in
        match cmpb flip a' b' with
        | Ord.EQ ->
            let open Ord.StructOrd (Char) in
            compare a'' b''
        | o -> o
    in
    match (p1, p2) with
    | P, P -> cmpb false x y
    | N, N -> cmpb true x y
    | P, N -> Ord.GT
    | N, P -> Ord.LT

  let rec add (BigInt (p1, a)) (BigInt (p2, b)) =
    match (p1, p2) with
    | P, P ->
        let rec adc a b =
          match (is_zero a, is_zero b) with
          | true, _ -> (a, 0)
          | false, true -> (b, 1)
          | false, false ->
              let a', a'' = bsplitb a in
              let b', b'' = bsplitb b in
              let r, c = adc (normalize a') (normalize b') in
              let r' = int_of_char a'' + int_of_char b'' + c in
              if r' > 10 then (app (r' mod 10) r, Int.div r' 10)
              else (app r' r, 0)
        in
        let r, c = adc a b in
        BigInt (P, (if c > 0 then app c r else r) |> normalize)
    | P, N -> sub (BigInt (P, a)) (BigInt (P, b))
    | N, P -> sub (BigInt (P, b)) (BigInt (P, a))
    | N, N ->
        let (BigInt (_, r)) = add (BigInt (P, a)) (BigInt (P, b)) in
        BigInt (N, r)

  and sub (BigInt (p1, a)) (BigInt (p2, b)) =
    match (p1, p2) with
    | P, P ->
        let rec subc a b =
          match (is_zero a, is_zero b) with
          | true, _ -> (a, 0)
          | false, true -> (b, 1)
          | false, false ->
              let a', a'' = bsplitb a in
              let b', b'' = bsplitb b in
              let r, c = subc (normalize a') (normalize b') in
              let r' = int_of_char a'' - int_of_char b'' - c in
              let r, c =
                if r' < 0 then (app (-(-r' mod 10)) r, 1) else (app r' r, 0)
              in
              (r, c)
        in
        let r, c = subc a b in
        if c > 0 then
          (* HACK: Figure this out *)
          let r, _ = subc b a in
          BigInt (N, normalize r)
        else BigInt (P, normalize r)
    | P, N -> add (BigInt (P, a)) (BigInt (P, b))
    | N, P ->
        let (BigInt (s, r)) = add (BigInt (P, a)) (BigInt (P, b)) in
        BigInt (revsign s, r)
    | N, N -> sub (BigInt (P, a)) (BigInt (P, b))

  let mul (BigInt (p1, a)) (BigInt (p2, b)) =
    let sign = match (p1, p2) with P, P | N, N -> P | P, N | N, P -> N in
    let r =
      let a' = Bytes.to_seq a |> Seq.map int_of_char |> List.of_seq in
      let b' = Bytes.to_seq b |> Seq.map int_of_char |> List.of_seq in
      let open Monad.Make (Minicat_ext.List_ext) in
      let* x = a' in
      let* y = b' in
      Printf.eprintf "%d * %d = %d; " x y (x * y);
      pure (x * y)
    in
    Printf.eprintf "\n";
    let inner x (b, c) =
      let q, r = Minicat_ext.Int_ext.quotrem (x + c) 10 in
      Printf.eprintf "%d (%d); " r q;
      (prep r b, q)
    in
    let b =
      BigInt
        ( sign,
          List.fold_right inner r (Bytes.empty, 0)
          |> (fun (x, c) -> prep c x)
          |> normalize )
    in
    Printf.eprintf "\n";
    b

  let abs (BigInt (_, x)) = BigInt (P, x)

  let signum (BigInt (p, _)) = if p = P then of_string "1" else of_string "-1"

  let quotrem _ = failwith "Not implemented"
end

include Integer.Make (Inner)
