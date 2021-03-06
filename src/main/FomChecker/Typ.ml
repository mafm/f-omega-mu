open FomDiag
open FomBasis

(* *)

include FomSyntax.Typ

(* *)

module Env = Map.Make (Id)

let rec check typ : _ -> Kind.t =
  let open Reader in
  let quantifier symbol f =
    let* f_kind = check f in
    match f_kind with
    | `Arrow (_, _, (`Star _ as c_kind)) -> return c_kind
    | _ -> Error.quantifier_kind (at typ) symbol f f_kind
  in
  match typ with
  | `Mu (at', f) -> (
    let* f_kind = check f in
    match f_kind with
    | `Star _ -> Error.mu_kind at' f f_kind
    | `Arrow (_, d_kind, c_kind) ->
      if not (Kind.equal d_kind c_kind) then
        Error.mu_kind at' f f_kind;
      return c_kind)
  | `Const (at', c) -> return (Const.kind_of at' c)
  | `Var (at', i) -> (
    let* i_kind_opt e = Env.find_opt i e#get_typ_env in
    match i_kind_opt with
    | None -> Error.typ_var_unbound at' i
    | Some (def, i_kind) ->
      let* () = Annot.Typ.use i def in
      return i_kind)
  | `Lam (at', d, d_kind, r) ->
    let* () = Annot.Typ.def d d_kind in
    let* r_kind e = Env.add d (d, d_kind) |> e#map_typ_env |> check r in
    return (`Arrow (at', d_kind, r_kind))
  | `App (at', f, x) -> (
    let* f_kind = check f in
    match f_kind with
    | `Star _ -> Error.app_of_kind_star at' f x
    | `Arrow (_, d_kind, c_kind) ->
      let* x_kind = check x in
      if not (Kind.equal d_kind x_kind) then
        Error.app_kind_mismatch at' f d_kind x x_kind;
      return c_kind)
  | `ForAll (_, f) -> quantifier FomPP.for_all f
  | `Exists (_, f) -> quantifier FomPP.exists f

let rec norm typ =
  match typ with
  | `Mu (at, typ') -> `Mu (at, norm typ')
  | `Const _ -> typ
  | `Var _ -> typ
  | `Lam (at, id, kind, typ') -> (
    let typ'' = norm typ' in
    match typ'' with
    | `App (_, fn, `Var (_, id')) when Id.equal id id' && not (is_free id fn) ->
      fn
    | _ -> `Lam (at, id, kind, typ''))
  | `App (at, fn, arg) -> (
    let fn' = norm fn in
    let arg' = norm arg in
    match fn' with
    | `Lam (_, id, _, body) -> norm (subst id arg' body)
    | _ -> `App (at, fn', arg'))
  | `ForAll (at, typ') -> `ForAll (at, norm typ')
  | `Exists (at, typ') -> `Exists (at, norm typ')

let rec head_of_norm typ =
  match typ with
  | `Mu (at, f) -> head_of_norm (norm (`App (at, f, typ)))
  | _ -> typ

module Ids = Set.Make (Id)

let rec is_contractive ids typ =
  match typ with
  | `Mu (_, `Lam (_, id, `Star _, `Var (_, id')))
    when Id.equal id' id || Ids.mem id' ids ->
    false
  | `Mu (_, `Lam (_, id, `Star _, body)) -> is_contractive (Ids.add id ids) body
  | _ -> true

let is_contractive = is_contractive Ids.empty

module Set = Set.Make (Compare.Pair (FomSyntax.Typ) (FomSyntax.Typ))

let support (lhs, rhs) =
  match (lhs, rhs) with
  | `Const (_, lhs_const), `Const (_, rhs_const)
    when Const.equal lhs_const rhs_const ->
    Some Set.empty
  | `Var (_, lhs_id), `Var (_, rhs_id) when Id.equal lhs_id rhs_id ->
    Some Set.empty
  | `ForAll (_, lhs), `ForAll (_, rhs) | `Exists (_, lhs), `Exists (_, rhs) ->
    Some (Set.singleton (lhs, rhs))
  | `App (_, lhs_fn, lhs_arg), `App (_, rhs_fn, rhs_arg) ->
    Some
      (Set.union
         (Set.singleton (lhs_fn, rhs_fn))
         (Set.singleton (lhs_arg, rhs_arg)))
  | `Lam (_, lhs_id, lhs_kind, lhs_typ), `Lam (_, rhs_id, rhs_kind, rhs_typ)
    when Kind.equal lhs_kind rhs_kind ->
    Some
      (Set.singleton
         (if Id.equal lhs_id rhs_id then
            (lhs_typ, rhs_typ)
         else
           let new_var = `Var (at lhs, Id.freshen lhs_id) in
           (subst lhs_id new_var lhs_typ, subst rhs_id new_var rhs_typ)))
  | `Mu (_, `Lam (_, _, `Star _, _)), `Mu (_, `Lam (_, _, `Star _, _))
    when (not (is_contractive lhs)) && not (is_contractive rhs) ->
    Some Set.empty
  | `Mu (_, `Lam (_, lhs_id, `Star _, lhs_typ)), _ when is_contractive lhs ->
    Some (Set.singleton (norm (subst lhs_id lhs lhs_typ), rhs))
  | `Mu (_, lhs_typ), _ ->
    Some (Set.singleton (norm (`App (at lhs, lhs_typ, lhs)), rhs))
  | _, `Mu (_, `Lam (_, rhs_id, `Star _, rhs_typ)) when is_contractive rhs ->
    Some (Set.singleton (lhs, norm (subst rhs_id rhs rhs_typ)))
  | _, `Mu (_, rhs_typ) ->
    Some (Set.singleton (lhs, norm (`App (at rhs, rhs_typ, rhs))))
  | _ -> None

let rec gfp a xs =
  Set.is_empty xs
  ||
  let x = Set.choose xs in
  if Set.mem x a then
    gfp a (Set.remove x xs)
  else
    match support x with
    | None -> false
    | Some s -> gfp (Set.add x a) (Set.union xs s)

let equal_of_norm lhs rhs = gfp Set.empty (Set.singleton (lhs, rhs))
