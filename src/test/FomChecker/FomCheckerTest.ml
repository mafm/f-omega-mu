open FomTest
open FomChecker
open FomParser

let parse_typ = parse_utf_8 Grammar.typ_exp Lexer.plain
let parse_exp = parse_utf_8 Grammar.program Lexer.plain

let () =
  test "Typ.is_contractive" @@ fun () ->
  verify (Typ.is_contractive (parse_typ "μxs.x→xs"));
  verify (not (Typ.is_contractive (parse_typ "μxs.xs")))

let () =
  test "Typ.equal_of_norm" @@ fun () ->
  let eq t1 t2 =
    Typ.equal_of_norm (parse_typ t1 |> Typ.norm) (parse_typ t2 |> Typ.norm)
  in
  verify (eq "λx.μxs.x→xs" "λy.y→(μys.y→y→ys)");
  verify (eq "λx.x" "λy.μys.y");
  verify (eq "∀x.x→x" "∀y.y→y");
  verify (eq "λf:*→*.f" "λf:*→*.λy.(λx.f x) y");
  verify (eq "μx.x" "μx.μy.y");
  verify (not (eq "∀x.∀y.x→y" "∀y.∀x.x→y"));
  verify (not (eq "∀x.x→x" "∀y.y→y→y"));
  verify (not (eq "λx.μxs.x→xs" "λy.y→y"))

let () =
  test "Exp.check" @@ fun () ->
  parse_exp
    {eof|
    let fact =
      rec fact: int -> int =>
        fun n: int =>
          if n =[int] 0
          then 1
          else n * fact (n - 1) in
    fact 5
    |eof}
  |> Exp.check Typ.Env.empty Exp.Env.empty
  |> ignore
