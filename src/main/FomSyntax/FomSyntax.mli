open FomSource

module Kind : sig
  type 't f = Star | Arrow of 't * 't
  type t = {it : t f; at : Loc.t}

  (* Constructors *)

  val star : Loc.t -> t
  val arrow : Loc.t -> t -> t -> t

  (* Comparison *)

  val compare : t -> t -> int

  (* Formatting *)

  val pp : t -> FomPP.document
end

module Label : Id.S

module Typ : sig
  module Const : sig
    type t = Arrow | Bool | Int | Product of Label.t list

    (* Comparison *)

    val equal : t -> t -> bool
    val compare : t -> t -> int

    (* Kinding *)

    val kind_of : Loc.t -> t -> Kind.t

    (* Formatting *)

    val pp : t -> FomPP.document
  end

  module Id : Id.S

  type 't f =
    | Mu of 't
    | Const of Const.t
    | Var of Id.t
    | Lam of Id.t * Kind.t * 't
    | App of 't * 't
    | ForAll of 't

  type t = {it : t f; at : Loc.t}

  (* Constructors *)

  val mu : Loc.t -> t -> t
  val const : Loc.t -> Const.t -> t
  val var : Loc.t -> Id.t -> t
  val lam : Loc.t -> Id.t -> Kind.t -> t -> t
  val app : Loc.t -> t -> t -> t
  val arrow : Loc.t -> t -> t -> t
  val for_all : Loc.t -> t -> t
  val int : Loc.t -> t
  val bool : Loc.t -> t
  val product : Loc.t -> (Label.t * t) list -> t

  (* Comparison *)

  val compare : t -> t -> int

  (*  *)

  val linearize : t -> t * t list
  val arity_and_result : t -> int * t
  val is_int : t -> bool
  val is_bool : t -> bool

  (* Substitution *)

  val is_free : Id.t -> t -> bool
  val subst : Id.t -> t -> t -> t

  (* Formatting *)

  val pp : t -> FomPP.document
end

module Exp : sig
  module Const : sig
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

    val type_of : Loc.t -> t -> Typ.t
  end

  module Id : Id.S

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

  val const : Loc.t -> Const.t -> t
  val var : Loc.t -> Id.t -> t
  val lam : Loc.t -> Id.t -> Typ.t -> t -> t
  val app : Loc.t -> t -> t -> t
  val gen : Loc.t -> Typ.Id.t -> Kind.t -> t -> t
  val inst : Loc.t -> t -> Typ.t -> t
  val fix : Loc.t -> t -> t
  val let_in : Loc.t -> Id.t -> t -> t -> t
  val if_else : Loc.t -> t -> t -> t -> t
  val bin_op : Loc.t -> t -> t -> t -> t
  val product : Loc.t -> (Label.t * t) list -> t
  val select : Loc.t -> t -> Label.t -> t

  (* Macros *)

  val let_typ_in : Typ.Id.t -> Typ.t -> t -> t

  (* Constants *)

  val lit_bool : Loc.t -> bool -> t
  val lit_nat : Loc.t -> Bigint.t -> t
  val op_arith_add : Loc.t -> t
  val op_arith_div : Loc.t -> t
  val op_arith_minus : Loc.t -> t
  val op_arith_mul : Loc.t -> t
  val op_arith_plus : Loc.t -> t
  val op_arith_rem : Loc.t -> t
  val op_arith_sub : Loc.t -> t
  val op_cmp_gt : Loc.t -> t
  val op_cmp_gt_eq : Loc.t -> t
  val op_cmp_lt : Loc.t -> t
  val op_cmp_lt_eq : Loc.t -> t
  val op_eq : Loc.t -> Typ.t -> t
  val op_eq_not : Loc.t -> Typ.t -> t
  val op_logical_and : Loc.t -> t
  val op_logical_not : Loc.t -> t
  val op_logical_or : Loc.t -> t
end
