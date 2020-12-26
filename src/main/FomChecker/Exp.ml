open FomBasis
include FomSyntax.Exp
module Env = Map.Make (Id)

let rec check typ_env env {it; at} : Typ.t =
  match it with
  | Const const -> Const.type_of at const
  | Var id -> (
    match Env.find_opt id env with
    | None -> failwithf "Unbound variable \"%s\"" id.it
    | Some typ -> typ)
  | Lam (id, dom, exp) -> (
    let dom_kind = Typ.check typ_env dom in
    match dom_kind.it with
    | Arrow _ -> failwith "Non * kind for function"
    | Star ->
      let dom = Typ.norm dom in
      Typ.arrow at dom (check typ_env (Env.add id dom env) exp))
  | App (fn, arg) -> (
    let fn_typ = Typ.head_of_norm (check typ_env env fn) in
    match fn_typ.it with
    | App ({it = App ({it = Const Arrow; _}, dom); _}, cod) ->
      let arg_typ = check typ_env env arg in
      if not (Typ.equal_of_norm dom arg_typ) then
        failwithf "Function argument (%s) and parameter (%s) mismatch"
          (Typ.pp dom |> FomPP.to_string)
          (Typ.pp arg_typ |> FomPP.to_string);
      cod
    | _ -> failwith "Function not of _→_ type")
  | Gen (id, dom, exp) ->
    Typ.for_all at
      (Typ.norm
         (Typ.lam at id dom (check (Typ.Env.add id dom typ_env) env exp)))
  | Inst (fn, arg) -> (
    let fn_typ = Typ.head_of_norm (check typ_env env fn) in
    match fn_typ.it with
    | ForAll typ -> (
      let fn_kind = Typ.check typ_env typ in
      match fn_kind.it with
      | Star -> failwith "Invalid * kind for ∀ type constructor"
      | Arrow (dom, _) ->
        let arg_kind = Typ.check typ_env arg in
        if not (Kind.equal dom arg_kind) then
          failwith "∀ argument and parameter kind mismatch";
        Typ.norm (Typ.app at typ arg))
    | _ -> failwith "Expression of non ∀_:_._ type cannot be instantiated")
  | LetIn (id, exp, body) ->
    check typ_env (Env.add id (check typ_env env exp) env) body
  | Fix exp -> (
    let typ = Typ.head_of_norm (check typ_env env exp) in
    match typ.it with
    | App ({it = App ({it = Const Arrow; _}, dom); _}, cod) ->
      if not (Typ.equal_of_norm dom cod) then
        failwith "μ expression not of t→t type";
      cod
    | _ -> failwith "μ expression not of _→_ type")
  | IfElse (c_exp, t_exp, e_exp) ->
    let c_typ = Typ.head_of_norm (check typ_env env c_exp) in
    if Typ.is_bool c_typ then (
      let t_typ = check typ_env env t_exp in
      let e_typ = check typ_env env e_exp in
      if not (Typ.equal_of_norm t_typ e_typ) then
        failwith "Then and else branches have different types";
      t_typ)
    else
      failwithf "Condition type (%s) not bool" (Typ.pp c_typ |> FomPP.to_string)
  | Product fields ->
    fields
    |> List.map (fun (label, exp) -> (label, check typ_env env exp))
    |> Typ.product at
  | Select (exp, label) -> (
    let exp_typ = Typ.head_of_norm (check typ_env env exp) in
    match Typ.linearize exp_typ with
    | {it = Const (Product labels); _}, typs -> (
      match
        List.combine labels typs
        |> List.find_opt (fun (label', _) -> Label.equal label' label)
      with
      | Some (_, typ) -> typ
      | None -> failwith "Select target lacks selected label")
    | _ -> failwith "Select target not a product type")
