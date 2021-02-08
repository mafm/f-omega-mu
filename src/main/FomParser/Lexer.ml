open FomBasis
open FomSource
open Grammar

let return_from buffer tok =
  let lhs, rhs = Buffer.loc buffer in
  (tok, lhs, rhs)

let lambda_lower = [%sedlex.regexp? 0x03bb (* λ *)]
let lambda_upper = [%sedlex.regexp? 0x039b (* Λ *)]
let arrow_right = [%sedlex.regexp? 0x2192 (* → *)]
let mu_lower = [%sedlex.regexp? 0x03bc (* μ *)]
let for_all = [%sedlex.regexp? 0x2200 (* ∀ *)]
let less_equal = [%sedlex.regexp? 0x2264 (* ≤ *)]
let greater_equal = [%sedlex.regexp? 0x2266 (* ≥ *)]
let logical_and = [%sedlex.regexp? 0x2227 (* ∧ *)]
let logical_or = [%sedlex.regexp? 0x2228 (* ∨ *)]
let logical_not = [%sedlex.regexp? 0x00ac (* ¬ *)]
let not_equal = [%sedlex.regexp? 0x2200 (* ≠ *)]
let comment = [%sedlex.regexp? "//", Star (Compl ('\n' | '\r'))]
let whitespace = [%sedlex.regexp? Plus (Chars " \t\n\r")]

let id =
  [%sedlex.regexp?
    ( Sub (tr8876_ident_char, (lambda_lower | lambda_upper | mu_lower)),
      Star (tr8876_ident_char | '_' | '0' .. '9') )]

let nat_10 = [%sedlex.regexp? "0" | '1' .. '9', Star '0' .. '9']

let rec token_or_comment buffer =
  let return = return_from buffer in
  match%sedlex buffer with
  | "!=" -> return NotEqual
  | "%" -> return Percent
  | "(" -> return ParenLhs
  | ")" -> return ParenRhs
  | "*" -> return Star
  | "+" -> return Plus
  | "," -> return Comma
  | "-" -> return Minus
  | "->" | arrow_right -> return ArrowRight
  | "/" -> return Slash
  | ":" -> return Colon
  | "<" -> return Less
  | "=" -> return Equal
  | "=>" | "." -> return Dot
  | ">" -> return Greater
  | "[" -> return BracketLhs
  | "]" -> return BracketRhs
  | "{" -> return BraceLhs
  | "}" -> return BraceRhs
  (* *)
  | "and" -> return And
  | "bool" -> return Bool
  | "else" -> return Else
  | "false" -> return (BoolLit false)
  | "forall" | for_all -> return ForAll
  | "if" -> return If
  | "in" -> return In
  | "int" -> return Int
  | "let" -> return Let
  | "then" -> return Then
  | "true" -> return (BoolLit true)
  | "type" -> return Type
  | greater_equal | ">=" -> return GreaterEqual
  | lambda_lower | "fun" -> return LambdaLower
  | lambda_upper | "gen" -> return LambdaUpper
  | less_equal | "<=" -> return LessEqual
  | logical_and | "&&" -> return LogicalAnd
  | logical_not | "!" -> return LogicalNot
  | logical_or | "||" -> return LogicalOr
  | mu_lower | "rec" -> return MuLower
  | nat_10 -> return (NatLit (Buffer.lexeme_utf_8 buffer |> Bigint.of_string))
  (* *)
  | id -> return (Id (Buffer.lexeme_utf_8 buffer))
  (* *)
  | comment -> return (Comment (Buffer.lexeme_utf_8 buffer))
  | whitespace -> token_or_comment buffer
  (* *)
  | eof -> return EOF
  (* *)
  | nat_10, id | any ->
    failwithf "%s: Syntax error: \"…%s…\""
      (Buffer.loc buffer |> Loc.to_message)
      (Buffer.lexeme_utf_8 buffer)
  | _ -> failwithf "%s: Syntax error" (Buffer.loc buffer |> Loc.to_message)

let rec token buffer =
  match token_or_comment buffer with
  | Comment _, _, _ -> token buffer
  | other -> other

type t = Buffer.t -> unit -> token * Lexing.position * Lexing.position

let plain : t = fun buffer () -> token buffer

type token_info = {begins : int; ends : int; name : string}

let number = "number"
let keyword = "keyword"
let punctuation = "punctuation"
let builtin = "builtin"
let variable = "variable-2"
let atom = "atom"
let error = "error"
let operator = "operator"
let comment = "comment"

let token_info_utf_8 input =
  Buffer.from_utf_8 input |> token_or_comment |> fun (token, lhs, rhs) ->
  {
    begins = lhs.pos_cnum - lhs.pos_bol;
    ends = rhs.pos_cnum - lhs.pos_bol;
    name =
      (match token with
      | And -> keyword
      | ArrowRight -> operator
      | Bool -> builtin
      | BoolLit _ -> atom
      | BraceLhs -> punctuation
      | BraceRhs -> punctuation
      | BracketLhs -> punctuation
      | BracketRhs -> punctuation
      | Colon -> punctuation
      | Comma -> punctuation
      | Comment _ -> comment
      | Dot -> operator
      | EOF -> error
      | Else -> keyword
      | Equal -> operator
      | ForAll -> keyword
      | Greater -> operator
      | GreaterEqual -> operator
      | Id _ -> variable
      | If -> keyword
      | In -> keyword
      | Int -> builtin
      | LambdaLower -> keyword
      | LambdaUpper -> keyword
      | Less -> operator
      | LessEqual -> operator
      | Let -> keyword
      | LogicalAnd -> operator
      | LogicalNot -> operator
      | LogicalOr -> operator
      | Minus -> operator
      | MuLower -> keyword
      | NatLit _ -> number
      | NotEqual -> operator
      | ParenLhs -> punctuation
      | ParenRhs -> punctuation
      | Percent -> operator
      | Plus -> operator
      | Slash -> operator
      | Star -> operator
      | Then -> keyword
      | Type -> keyword);
  }
