open FomBasis
open FomSyntax

type cstr = Str of string | Join of cstr * cstr

let str lit = Str lit
let ( ^ ) lhs rhs = Join (lhs, rhs)

let to_string cstr =
  let buffer = Buffer.create 1000 in
  let rec doit = function
    | Str str -> Buffer.add_string buffer str
    | Join (lhs, rhs) ->
      doit lhs;
      doit rhs
  in
  doit cstr;
  Buffer.contents buffer

module Label = struct
  include Label

  let to_js (label : t) = str label.it
end

module Exp = struct
  include Exp

  module Const = struct
    include Const

    let bi_2_pow_31, bi_2_pow_32, bi_2_pow_32_minus_1 =
      let open Bigint in
      ( shift_left (of_int 1) 31,
        shift_left (of_int 1) 32,
        shift_left (of_int 1) 32 - of_int 1 )

    let to_js c =
      match c with
      | LitBool bool -> Bool.to_string bool |> str
      | LitNat nat ->
        let open Bigint in
        let nat = bit_and nat bi_2_pow_32_minus_1 in
        (* TODO: Warn when literal is truncated. *)
        if nat < bi_2_pow_31 then
          nat |> to_string |> str
        else
          str "(" ^ (nat - bi_2_pow_32 |> to_string |> str) ^ str ")"
      | OpArithAdd -> str "+"
      | OpArithDiv -> str "/"
      | OpArithMinus -> str "-"
      | OpArithMul -> str "*"
      | OpArithPlus -> str "+"
      | OpArithRem -> str "%"
      | OpArithSub -> str "-"
      | OpCmpGt -> str ">"
      | OpCmpGtEq -> str ">="
      | OpCmpLt -> str "<"
      | OpCmpLtEq -> str "<="
      | OpEq _ -> str "==="
      | OpEqNot _ -> str "!=="
      | OpLogicalAnd -> str "&&"
      | OpLogicalNot -> str "!"
      | OpLogicalOr -> str "||"
  end

  module Id = struct
    include Id

    let to_js ({it; _} : t) =
      if Js.is_illegal_id it then
        str it ^ str "$"
      else
        str it
  end

  let linearize exp =
    let rec recurse exp =
      match exp.it with
      | App (f, x) ->
        let f, xs = recurse f in
        (f, x :: xs)
      | _ -> (exp, [])
    in
    let f, xs = recurse exp in
    (f, List.rev xs)

  let coerce_to_int exp = exp ^ str " | 0"
  let coerce_to_int_if bool exp = if bool then coerce_to_int exp else exp

  let rec to_js ({it; at} as exp : t) =
    match it with
    | Const c -> (
      match Const.type_of at c |> Typ.arity_and_result with
      | 2, result ->
        str "(x => y => "
        ^ coerce_to_int_if (Typ.is_int result)
            (str "x " ^ Const.to_js c ^ str " y")
        ^ str ")"
      | 1, result ->
        str "(x => "
        ^ coerce_to_int_if (Typ.is_int result) (Const.to_js c ^ str " x")
        ^ str ")"
      | 0, _ -> Const.to_js c
      | n, _ -> failwithf "Unsupported arity %d" n)
    | Var i -> Id.to_js i
    | Lam (i, _, e) -> str "(" ^ Id.to_js i ^ str " => " ^ to_js e ^ str ")"
    | App (f, x) -> (
      let default () = to_js f ^ str "(" ^ to_js x ^ str ")" in
      let op, xs = linearize exp in
      match op with
      | {it = Const c; _} -> (
        let n, result = Const.type_of at c |> Typ.arity_and_result in
        match (n, xs) with
        | 2, [lhs; rhs] ->
          str "("
          ^ coerce_to_int_if (Typ.is_int result)
              (to_js lhs ^ str " " ^ Const.to_js c ^ str " " ^ to_js rhs)
          ^ str ")"
        | 1, [rhs] ->
          str "("
          ^ coerce_to_int_if (Typ.is_int result) (Const.to_js c ^ to_js rhs)
          ^ str ")"
        | _ -> default ())
      | _ -> default ())
    | Gen (_, _, e) -> to_js e
    | Inst (e, _) -> to_js e
    | LetIn (i, v, e) ->
      str "(" ^ Id.to_js i ^ str " => " ^ to_js e ^ str ")(" ^ to_js v ^ str ")"
    | Fix {it = Lam (f, _, {it = Lam (x, _, e); _}); _} ->
      str "function " ^ Id.to_js f ^ str "(" ^ Id.to_js x ^ str ") {return "
      ^ to_js e ^ str "}"
    | Fix f -> str "fix(" ^ to_js f ^ str ")"
    | IfElse (c, t, e) ->
      str "(" ^ to_js c ^ str " ? " ^ to_js t ^ str " : " ^ to_js e ^ str ")"
    | Product fs ->
      (fs
      |> List.map (fun (l, e) -> Label.to_js l ^ str ": " ^ to_js e)
      |> List.fold_left (fun es e -> es ^ e ^ str ", ") (str "({"))
      ^ str "})"
    | Select (e, l) -> to_js e ^ str "." ^ Label.to_js l
end

let to_js exp = to_string (Exp.to_js exp)
