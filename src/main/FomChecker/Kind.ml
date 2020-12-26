include FomSyntax.Kind

let rec equal lhs rhs =
  match (lhs.it, rhs.it) with
  | Star, Star -> true
  | Arrow (lhs_dom, lhs_cod), Arrow (rhs_dom, rhs_cod) ->
    equal lhs_dom rhs_dom && equal lhs_cod rhs_cod
  | _ -> false
