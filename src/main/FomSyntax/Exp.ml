open FomSource

module Const = struct
  type t =
    | LitBool of bool
    | LitNat of Bigint.t
    | OpArithAdd
    | OpArithDiv
    | OpArithMinus
    | OpArithMul
    | OpArithPlus
    | OpArithRem
    | OpArithSub
    | OpCmpGt
    | OpCmpGtEq
    | OpCmpLt
    | OpCmpLtEq
    | OpEq of Typ.t
    | OpEqNot of Typ.t
    | OpLogicalAnd
    | OpLogicalNot
    | OpLogicalOr

  (* Typing *)

  let type_of =
    let bool = Typ.bool Loc.dummy in
    let int = Typ.int Loc.dummy in
    fun at -> function
      | LitBool _ -> Typ.bool at
      | LitNat _ -> Typ.int at
      | OpArithAdd | OpArithSub | OpArithMul | OpArithDiv | OpArithRem ->
        Typ.arrow at int (Typ.arrow at int int)
      | OpArithPlus | OpArithMinus -> Typ.arrow at int int
      | OpCmpLt | OpCmpLtEq | OpCmpGt | OpCmpGtEq ->
        Typ.arrow at int (Typ.arrow at int bool)
      | OpEq typ | OpEqNot typ -> Typ.arrow at typ (Typ.arrow at typ bool)
      | OpLogicalAnd | OpLogicalOr -> Typ.arrow at bool (Typ.arrow at bool bool)
      | OpLogicalNot -> Typ.arrow at bool bool

  let lit_false = LitBool false
  let lit_true = LitBool true
end

module Id = Id.Make ()

type 't f =
  | Const of Const.t
  | Var of Id.t
  | Lam of Id.t * Typ.t * 't
  | App of 't * 't
  | Gen of Typ.Id.t * Kind.t * 't
  | Inst of 't * Typ.t
  | LetIn of Id.t * 't * 't
  | Fix of 't
  | IfElse of 't * 't * 't
  | Product of (Label.t * 't) list
  | Select of 't * Label.t

type t = {it : t f; at : Loc.t}

(* Constructors *)

let const at const = {it = Const const; at}
let var at id = {it = Var id; at}
let lam at id typ exp = {it = Lam (id, typ, exp); at}
let app at fn arg = {it = App (fn, arg); at}
let gen at id kind exp = {it = Gen (id, kind, exp); at}
let inst at fn arg = {it = Inst (fn, arg); at}
let fix at exp = {it = Fix exp; at}
let let_in at id vexp exp = {it = LetIn (id, vexp, exp); at}
let if_else at c t e = {it = IfElse (c, t, e); at}
let product at fields = {it = Product fields; at}
let select at exp label = {it = Select (exp, label); at}

let bin_op at lhs op rhs =
  match op.it with
  | Const OpLogicalAnd -> if_else at lhs rhs (const at Const.lit_false)
  | Const OpLogicalOr -> if_else at lhs (const at Const.lit_true) rhs
  | _ -> app at (app at op lhs) rhs

(* Macros *)

let rec let_typ_in id typ ({it; at} as exp) =
  match it with
  | Const _ -> exp
  | Var _ -> exp
  | Lam (i, t, e) -> lam at i (Typ.subst id typ t) (let_typ_in id typ e)
  | App (fn, arg) -> app at (let_typ_in id typ fn) (let_typ_in id typ arg)
  | Gen (i, k, e) ->
    if Typ.Id.equal i id then
      exp
    else if Typ.is_free i typ then
      let i' = Typ.Id.freshen i in
      let vi' = Typ.var at i' in
      gen at i' k (let_typ_in id typ (let_typ_in i' vi' exp))
    else
      gen at i k (let_typ_in id typ e)
  | Inst (e, t) -> inst at (let_typ_in id typ e) (Typ.subst id typ t)
  | LetIn (i, v, e) -> let_in at i (let_typ_in id typ v) (let_typ_in id typ e)
  | Fix exp -> fix at (let_typ_in id typ exp)
  | IfElse (c, t, e) ->
    if_else at (let_typ_in id typ c) (let_typ_in id typ t) (let_typ_in id typ e)
  | Product fs ->
    fs |> List.map (fun (l, e) -> (l, let_typ_in id typ e)) |> product at
  | Select (e, l) -> select at (let_typ_in id typ e) l

(* Constants *)

let lit_bool at value =
  const at (if value then Const.lit_true else Const.lit_false)

let lit_nat at value = const at (LitNat value)
let op_arith_add at = const at OpArithAdd
let op_arith_div at = const at OpArithDiv
let op_arith_minus at = const at OpArithMinus
let op_arith_mul at = const at OpArithMul
let op_arith_plus at = const at OpArithPlus
let op_arith_rem at = const at OpArithRem
let op_arith_sub at = const at OpArithSub
let op_cmp_gt at = const at OpCmpGt
let op_cmp_gt_eq at = const at OpCmpGtEq
let op_cmp_lt at = const at OpCmpLt
let op_cmp_lt_eq at = const at OpCmpLtEq
let op_eq at typ = const at (OpEq typ)
let op_eq_not at typ = const at (OpEqNot typ)
let op_logical_and at = const at OpLogicalAnd
let op_logical_not at = const at OpLogicalNot
let op_logical_or at = const at OpLogicalOr
