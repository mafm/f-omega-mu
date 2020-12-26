open FomBasis
open FomSource

module Const = struct
  type t = Arrow | Bool | Int | Product of Label.t list

  (* Comparison *)

  let equal lhs rhs =
    match (lhs, rhs) with
    | Arrow, Arrow | Bool, Bool | Int, Int -> true
    | Product lhs, Product rhs -> ListExt.equal_with Label.equal lhs rhs
    | _ -> false

  let index = function Arrow -> 0 | Bool -> 1 | Int -> 2 | Product _ -> 3

  let compare lhs rhs =
    match (lhs, rhs) with
    | Arrow, Arrow -> 0
    | Bool, Bool -> 0
    | Int, Int -> 0
    | Product lhs, Product rhs -> ListExt.compare_with Label.compare lhs rhs
    | _ -> index lhs - index rhs

  (* Kinding *)

  let kind_of at t =
    let star = Kind.star at in
    match t with
    | Arrow ->
      let arrow = Kind.arrow at in
      arrow star (arrow star star)
    | Bool | Int -> star
    | Product labels ->
      let arrow = Kind.arrow at in
      List.fold_left (fun result _ -> arrow star result) star labels

  (* Formatting *)

  let pp =
    let open FomPP in
    let int = string "int" in
    let bool = string "bool" in
    function
    | Arrow -> arrow_right
    | Bool -> bool
    | Int -> int
    | Product labels ->
      labels |> List.map Label.pp
      |> separate (concat [comma; break 1])
      |> braces
end

module Id = Id.Make ()

type 't f =
  | Mu of 't
  | Const of Const.t
  | Var of Id.t
  | Lam of Id.t * Kind.t * 't
  | App of 't * 't
  | ForAll of 't

type t = {it : t f; at : Loc.t}

(* Constructors *)

let mu at typ = {it = Mu typ; at}
let const at const = {it = Const const; at}
let var at id = {it = Var id; at}
let lam at id kind typ = {it = Lam (id, kind, typ); at}
let app at fn arg = {it = App (fn, arg); at}
let arrow at dom cod = app at (app at (const at Arrow) dom) cod
let for_all at typ = {it = ForAll typ; at}
let bool at = const at Bool
let int at = const at Int

let product at fields =
  let fields = List.sort (Compare.the fst Label.compare) fields in
  let labels = List.map fst fields in
  let rec check_dups = function
    | l1 :: l2 :: ls ->
      if Label.equal l1 l2 then
        failwithf "Duplicate label: %s" l1.it;
      check_dups (l2 :: ls)
    | _ -> ()
  in
  check_dups labels;
  let typs = List.map snd fields in
  let ctor = const at (Product labels) in
  List.fold_left (app at) ctor typs

(* Comparison *)

let index t =
  match t.it with
  | Mu _ -> 0
  | Const _ -> 1
  | Var _ -> 2
  | Lam _ -> 3
  | App _ -> 4
  | ForAll _ -> 5

let rec compare lhs rhs =
  match (lhs.it, rhs.it) with
  | Mu lhs, Mu rhs -> compare lhs rhs
  | Const lhs, Const rhs -> Const.compare lhs rhs
  | Var lhs, Var rhs -> Id.compare lhs rhs
  | Lam (lhs_id, lhs_kind, lhs_typ), Lam (rhs_id, rhs_kind, rhs_typ) ->
    Id.compare lhs_id rhs_id <>? fun () ->
    Kind.compare lhs_kind rhs_kind <>? fun () -> compare lhs_typ rhs_typ
  | App (lhs_fn, lhs_arg), App (rhs_fn, rhs_arg) ->
    compare lhs_fn rhs_fn <>? fun () -> compare lhs_arg rhs_arg
  | ForAll lhs, ForAll rhs -> compare lhs rhs
  | _ -> index lhs - index rhs

(* *)

let linearize typ =
  let rec recurse typ =
    match typ.it with
    | App (f, x) ->
      let f, xs = recurse f in
      (f, x :: xs)
    | _ -> (typ, [])
  in
  let f, xs = recurse typ in
  (f, List.rev xs)

let rec arity_and_result typ =
  match typ.it with
  | App ({it = App ({it = Const Arrow; _}, _); _}, result) ->
    let n, result = arity_and_result result in
    (n + 1, result)
  | _ -> (0, typ)

(* Substitution *)

let rec is_free id {it; _} =
  match it with
  | Mu typ -> is_free id typ
  | Const _ -> false
  | Var id' -> Id.equal id id'
  | Lam (id', _, body) -> (not (Id.equal id id')) && is_free id body
  | App (fn, arg) -> is_free id fn || is_free id arg
  | ForAll typ -> is_free id typ

let rec subst id the ({it; at} as inn) =
  match it with
  | Mu typ -> mu at (subst id the typ)
  | Const _ -> inn
  | Var id' -> if Id.equal id id' then the else inn
  | Lam (id', kind, body) ->
    if Id.equal id id' then
      inn
    else if is_free id' the then
      let id'' = Id.freshen id' in
      let vid'' = var at id'' in
      lam at id'' kind (subst id the (subst id' vid'' body))
    else
      lam at id' kind (subst id the body)
  | App (fn, arg) -> app at (subst id the fn) (subst id the arg)
  | ForAll typ -> for_all at (subst id the typ)

let is_int {it; _} = match it with Const Int -> true | _ -> false
let is_bool {it; _} = match it with Const Bool -> true | _ -> false

(* Formatting *)

let rec binding atomize head id kind body =
  let open FomPP in
  let kind_annot =
    match kind.Kind.it with
    | Star -> empty
    | _ -> [colon; break_0; Kind.pp kind] |> concat
  in
  [
    [head; Id.pp id; kind_annot] |> concat |> nest 1 |> group;
    [dot; break_0; pp false body] |> concat |> nest 1 |> group;
  ]
  |> concat |> parens_if atomize

and pp atomize typ =
  let open FomPP in
  match typ.it with
  | Mu {it = Lam (id, kind, body); _} -> binding atomize mu_lower id kind body
  | Mu typ -> [mu_lower; pp false typ] |> concat |> nest 1 |> parens_if atomize
  | Const const -> Const.pp const
  | Var id -> Id.pp id
  | Lam (id, kind, body) -> binding atomize lambda_lower id kind body
  | App (fn, arg) -> (
    match linearize typ with
    | {it = Const Arrow; _}, [dom; cod] ->
      [pp true dom; break_1; arrow_right; space; pp false cod]
      |> concat |> parens_if atomize
    | {it = Const (Product labels); _}, typs
      when List.length labels = List.length typs ->
      List.combine labels typs
      |> List.map (fun (label, typ) ->
             [Label.pp label; colon; break_1; pp false typ]
             |> concat |> nest 1 |> group)
      |> separate comma_break_1 |> braces
    | _ -> [pp true fn; break_1; pp true arg] |> concat |> group)
  | ForAll {it = Lam (id, kind, body); _} ->
    binding atomize for_all id kind body
  | ForAll typ ->
    [for_all; pp false typ] |> concat |> nest 1 |> parens_if atomize

let pp typ = pp false typ |> FomPP.group
