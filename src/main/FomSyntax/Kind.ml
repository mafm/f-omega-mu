open FomBasis
open FomSource

(* *)

type 't f = Star | Arrow of 't * 't
type t = {it : t f; at : Loc.t}

(* Constructors *)

let star at = {it = Star; at}
let arrow at dom cod = {it = Arrow (dom, cod); at}

(* Comparison *)

let index t = match t.it with Star -> 0 | Arrow _ -> 1

let rec compare lhs rhs =
  match (lhs.it, rhs.it) with
  | Star, Star -> 0
  | Arrow (lhs_dom, lhs_cod), Arrow (rhs_dom, rhs_cod) ->
    compare lhs_dom rhs_dom <>? fun () -> compare lhs_cod rhs_cod
  | _ -> index lhs - index rhs

(* Formatting *)

let rec pp atomize {it; _} =
  let open FomPP in
  match it with
  | Star -> star
  | Arrow (dom, cod) ->
    [pp true dom; break 0; arrow_right; pp false cod]
    |> concat |> parens_if atomize

let pp kind = pp false kind |> FomPP.group
