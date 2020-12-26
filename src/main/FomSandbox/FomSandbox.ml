open Js_of_ocaml
open FomChecker
open FomParser
open FomToJs

let () =
  Js.Unsafe.global##.fom :=
    object%js
      val process =
        fun (input : Js.js_string Js.t) ->
          try
            let syntax =
              Js.to_string input |> parse_utf_8 Grammar.program Lexer.plain
            in
            object%js
              val typ =
                syntax
                |> Exp.check Typ.Env.empty Exp.Env.empty
                |> Typ.pp
                |> FomPP.to_string ~max_width:80
                |> Js.string

              val js = syntax |> to_js |> Js.string
            end
          with exn ->
            object%js
              val typ =
                match exn with
                | Failure message -> Js.string message
                | exn -> Printexc.to_string exn |> Js.string

              val js = Js.string ""
            end

      val token =
        fun (input : Js.js_string Js.t) ->
          try
            let {Lexer.begins; ends; name} =
              input |> Js.to_string |> Lexer.token_info_utf_8
            in
            object%js
              val begins = begins

              val ends = ends

              val name = Js.string name
            end
          with _ ->
            object%js
              val begins = 0

              val ends = 0

              val name = Js.string "error"
            end
    end
