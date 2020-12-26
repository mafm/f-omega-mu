open FomBasis
include FomSyntax.Typ
module Env = Map.Make (Id)

let rec check env {it; at} : Kind.t =
  match it with
  | Mu typ -> (
    let kind = check env typ in
    match kind.it with
    | Star -> failwith "Kind * for μ type constructor"
    | Arrow (dom, cod) ->
      if not (Kind.equal dom cod) then
        failwith "Invalid μ type constructor";
      cod)
  | Const con -> Const.kind_of at con
  | Var id -> (
    match Env.find_opt id env with
    | None -> failwithf "Unbound type variable \"%s\"" id.it
    | Some kind -> kind)
  | Lam (id, dom, typ) -> Kind.arrow at dom (check (Env.add id dom env) typ)
  | App (fn, arg) -> (
    let fn_kind = check env fn in
    match fn_kind.it with
    | Star -> failwith "Kind * for type abstraction"
    | Arrow (dom, cod) ->
      let arg_kind = check env arg in
      if not (Kind.equal dom arg_kind) then
        failwith "Type argument and parameter kind mismatch";
      cod)
  | ForAll typ -> (
    let kind = check env typ in
    match kind.it with
    | Star -> failwith "Kind * for ∀ type constructor"
    | Arrow (_, cod) -> cod)

let rec norm ({it; at} as typ) =
  match it with
  | Mu typ' -> mu at (norm typ')
  | Const _ -> typ
  | Var _ -> typ
  | Lam (id, kind, typ') -> (
    let typ'' = norm typ' in
    match typ''.it with
    | App (fn, {it = Var id'; _}) when Id.equal id id' && not (is_free id fn) ->
      fn
    | _ -> lam at id kind typ'')
  | App (fn, arg) -> (
    let fn' = norm fn in
    let arg' = norm arg in
    match fn'.it with
    | Lam (id, _, body) -> norm (subst id arg' body)
    | _ -> app at fn' arg')
  | ForAll typ' -> for_all at (norm typ')

let rec head_of_norm typ =
  match typ.it with
  | Mu {it = Lam (id, {it = Star; _}, body); _} ->
    head_of_norm (norm (subst id typ body))
  | _ -> typ

module Ids = Set.Make (Id)

let rec is_contractive ids {it; _} =
  match it with
  | Mu {it = Lam (id, {it = Star; _}, {it = Var id'; _}); _}
    when Id.equal id' id || Ids.mem id' ids ->
    false
  | Mu {it = Lam (id, {it = Star; _}, body); _} ->
    is_contractive (Ids.add id ids) body
  | _ -> true

let is_contractive = is_contractive Ids.empty

module Set = Set.Make (Compare.Pair (FomSyntax.Typ) (FomSyntax.Typ))

let support (lhs, rhs) =
  match (lhs.it, rhs.it) with
  | Const lhs_const, Const rhs_const when Const.equal lhs_const rhs_const ->
    Some Set.empty
  | Var lhs_id, Var rhs_id when Id.equal lhs_id rhs_id -> Some Set.empty
  | ForAll lhs, ForAll rhs -> Some (Set.singleton (lhs, rhs))
  | App (lhs_fn, lhs_arg), App (rhs_fn, rhs_arg) ->
    Some
      (Set.union
         (Set.singleton (lhs_fn, rhs_fn))
         (Set.singleton (lhs_arg, rhs_arg)))
  | Lam (lhs_id, lhs_kind, lhs_typ), Lam (rhs_id, rhs_kind, rhs_typ)
    when Kind.equal lhs_kind rhs_kind ->
    Some
      (Set.singleton
         (if Id.equal lhs_id rhs_id then
            (lhs_typ, rhs_typ)
         else
           let new_var = var lhs.at (Id.freshen lhs_id) in
           (subst lhs_id new_var lhs_typ, subst rhs_id new_var rhs_typ)))
  | ( Mu {it = Lam (_, {it = Star; _}, _); _},
      Mu {it = Lam (_, {it = Star; _}, _); _} )
    when (not (is_contractive lhs)) && not (is_contractive rhs) ->
    Some Set.empty
  | Mu {it = Lam (lhs_id, {it = Star; _}, lhs_typ); _}, _
    when is_contractive lhs ->
    Some (Set.singleton (norm (subst lhs_id lhs lhs_typ), rhs))
  | Mu lhs_typ, _ -> Some (Set.singleton (norm (app lhs.at lhs_typ lhs), rhs))
  | _, Mu {it = Lam (rhs_id, {it = Star; _}, rhs_typ); _}
    when is_contractive rhs ->
    Some (Set.singleton (lhs, norm (subst rhs_id rhs rhs_typ)))
  | _, Mu rhs_typ -> Some (Set.singleton (lhs, norm (app rhs.at rhs_typ rhs)))
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
