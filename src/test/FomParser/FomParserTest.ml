open FomTest
open FomParser

let parse_program = parse_utf_8 Grammar.program Lexer.plain

let () =
  test "location info" @@ fun () ->
  match (parse_program "Λα:*.\n  λx:α.x").it with
  | Gen
      ( {it = "α"; at = {pos_lnum = 1; pos_bol = 0; pos_cnum = 1; _}, _},
        {it = Star; _},
        {
          it =
            Lam
              ( {it = "x"; at = {pos_lnum = 2; pos_bol = 6; pos_cnum = 9; _}, _},
                {it = Var {it = "α"; _}; _},
                {it = Var {it = "x"; _}; _} );
          _;
        } ) ->
    ()
  | _ -> verify false

let () =
  test "symbolic" @@ fun () ->
  match (parse_program "Λt.μdiverge:t→t.λx:t.diverge x").it with
  | Gen _ -> ()
  | _ -> verify false

let () =
  test "keywords" @@ fun () ->
  match
    (parse_program "gen t => rec diverge : t -> t => fun x : t => diverge x").it
  with
  | Gen _ -> ()
  | _ -> verify false
