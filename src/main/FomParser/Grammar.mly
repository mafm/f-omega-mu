%token <Bigint.t> LitNat
%token <bool> LitBool
%token <string> LitString

%token <string> Id
%token <string> Comment

%token ArrowRight "→"
%token Bool "bool"
%token BraceLhs "{"
%token BraceRhs "}"
%token BracketLhs "["
%token BracketRhs "]"
%token Case "case"
%token Colon ":"
%token Comma ","
%token Dot "."
%token DoubleAngleLhs "《"
%token DoubleAngleRhs "》"
%token EOF
%token Else "else"
%token Equal "="
%token Exists "∃"
%token ForAll "∀"
%token Greater ">"
%token GreaterEqual "≥"
%token If "if"
%token In "in"
%token Int "int"
%token LambdaLower "λ"
%token LambdaUpper "Λ"
%token Less "<"
%token LessEqual "≤"
%token Let "let"
%token LogicalAnd "∧"
%token LogicalNot "¬"
%token LogicalOr "∨"
%token Minus "-"
%token MuLower "μ"
%token NotEqual "≠"
%token ParenLhs "("
%token ParenRhs ")"
%token Percent "%"
%token Plus "+"
%token Slash "/"
%token Star "*"
%token String "string"
%token Then "then"
%token Type "type"
%token Underscore "_"

%left "∨"
%left "∧"
%nonassoc "=" "≠" "]"
%nonassoc "<" "≤" "≥" ">"
%left "case"
%left "+" "-"
%left "*" "/" "%"

%start <Exp.t> program
%start <Typ.t> typ_exp

%{
  open FomDiag
  open FomSyntax

  let check_lab_list fs =
    let rec check_dups = function
      | l1 :: (l2 :: _ as ls) ->
        if Label.equal l1 l2 then
          Error.duplicated_label l2.at l1;
        check_dups ls
      | _ -> fs
    in
    fs
    |> List.map fst
    |> List.stable_sort Label.compare
    |> check_dups
%}

%%

rev_list_1(elem, sep):
  | e=elem                                            {[e]}
  | es=rev_list_1(elem, sep) sep e=elem               {e::es}

list_1(elem, sep):
  | es=rev_list_1(elem, sep)                          {List.rev es}

list_n(elem, sep):
  | option(sep)                                       {[]}
  | es=rev_list_1(elem, sep) option(sep)              {List.rev es}

//

kind_atom:
  | "*"                                               {`Star $loc}
  | "(" k=kind ")"                                    {k}

kind:
  | k=kind_atom                                       {k}
  | d=kind_atom "→" c=kind                            {`Arrow ($loc, d, c)}

//

label:
  | i=Id                                              {Label.id $loc i}

lab_list(item):
  | ls=list_n(item, ",")                              {check_lab_list ls}

//

lab_typ:
  | l=label ":" t=typ                                 {(l, t)}
  | l=label                                           {(l, Typ.var_of_label l)}

typ_rid:
  | i=Id                                              {Typ.Id.id $loc i}

typ_bid:
  | Underscore                                        {Typ.Id.id $loc "_"}
  | i=typ_rid                                         {i}

typ_bind:
  | i=typ_bid                                         {(i, `Star $loc)}
  | i=typ_bid ":" k=kind                              {(i, k)}

typ_atom:
  | i=typ_rid                                         {`Var ($loc, i)}
  | "int"                                             {`Const ($loc, `Int)}
  | "bool"                                            {`Const ($loc, `Bool)}
  | "string"                                          {`Const ($loc, `String)}
  | "(" t=typ ")"                                     {t}
  | "{" fs=lab_list(lab_typ) "}"                      {Typ.product $loc fs}
  | "[" cs=lab_list(lab_typ) "]"                      {Typ.sum $loc cs}
  | "μ" "(" t=typ ")"                                 {`Mu ($loc, t)}
  | "∃" "(" t=typ ")"                                 {`Exists ($loc, t)}
  | "∀" "(" t=typ ")"                                 {`ForAll ($loc, t)}

typ_app:
  | t=typ_atom                                        {t}
  | f=typ_app x=typ_atom                              {`App ($loc, f, x)}

typ_inf:
  | t=typ_app                                         {t}
  | d=typ_app "→" c=typ                               {Typ.arrow $loc d c}

typ_lam(head):
  | head b=typ_bind "." t=typ                         {`Lam ($loc, fst b, snd b, t)}

typ:
  | t=typ_inf                                         {t}
  | t=typ_lam("μ")                                    {`Mu ($loc, t)}
  | t=typ_lam("∃")                                    {`Exists ($loc, t)}
  | t=typ_lam("∀")                                    {`ForAll ($loc, t)}
  | t=typ_lam("λ")                                    {t}

//

lab_exp:
  | l=label "=" e=exp                                 {(l, e)}
  | l=label                                           {(l, Exp.var_of_label l)}

exp_rid:
  | i=Id                                              {Exp.Id.id $loc i}

exp_bid:
  | Underscore                                        {Exp.Id.id $loc "_"}
  | i=exp_rid                                         {i}

exp_atom:
  | i=exp_rid                                         {`Var ($loc, i)}
  | l=LitBool                                         {Exp.lit_bool $loc l}
  | l=LitNat                                          {`Const ($loc, `LitNat l)}
  | l=LitString                                       {`Const ($loc, `LitString l)}
  | "(" e=exp ")"                                     {e}
  | "{" fs=lab_list(lab_exp) "}"                      {`Product ($loc, fs)}
  | "[" c=lab_exp ":" t=typ "]"                       {`Inject ($loc, fst c, snd c, t)}
  | "《" e=exp ":" f=typ "/" x=typ "》"               {`Pack ($loc, x, e, f)}
  | e=exp_atom "." l=label                            {`Select ($loc, e, l)}
  | "μ" "(" e=exp ")"                                 {`Mu ($loc, e)}

exp_app:
  | e=exp_atom                                        {e}
  | f=exp_app x=exp_atom                              {`App ($loc, f, x)}
  | f=exp_app "[" x=typ "]"                           {`Inst ($loc, f, x)}

exp_inf:
  | e=exp_app                                         {e}
  | f=uop x=exp_app                                   {`App ($loc, f, x)}
  | l=exp_inf o=bop r=exp_inf                         {Exp.bin_op $loc l o r}
  | e=exp_inf "case" cs=exp_inf                       {`Case ($loc, e, cs)}

%inline uop:
  | "¬"                                               {`Const ($loc, `OpLogicalNot)}
  | "+"                                               {`Const ($loc, `OpArithPlus)}
  | "-"                                               {`Const ($loc, `OpArithMinus)}

%inline bop:
  | "∨"                                               {`Const ($loc, `OpLogicalOr)}
  | "∧"                                               {`Const ($loc, `OpLogicalAnd)}

  | "=" "[" t=typ "]"                                 {`Const ($loc, `OpEq t)}
  | "≠" "[" t=typ "]"                                 {`Const ($loc, `OpEqNot t)}

  | ">"                                               {`Const ($loc, `OpCmpGt)}
  | "≥"                                               {`Const ($loc, `OpCmpGtEq)}
  | "<"                                               {`Const ($loc, `OpCmpLt)}
  | "≤"                                               {`Const ($loc, `OpCmpLtEq)}

  | "+"                                               {`Const ($loc, `OpArithAdd)}
  | "-"                                               {`Const ($loc, `OpArithSub)}

  | "*"                                               {`Const ($loc, `OpArithMul)}
  | "/"                                               {`Const ($loc, `OpArithDiv)}
  | "%"                                               {`Const ($loc, `OpArithRem)}

exp_bind(head):
  | head i=exp_bid ":" t=typ "." e=exp                {`Lam ($loc, i, t, e)}

exp:
  | e=exp_inf                                                   {e}
  | e=exp_bind("μ")                                             {`Mu ($loc, e)}
  | e=exp_bind("λ")                                             {e}
  | "Λ" b=typ_bind "." e=exp                                    {`Gen ($loc, fst b, snd b, e)}
  | "if" c=exp "then" t=exp "else" e=exp                        {`IfElse ($loc, c, t, e)}
  | "let" "type" i=typ_bid "=" t=typ "in" e=exp                 {Exp.let_typ_in i t e}
  | "let" i=exp_bid "=" v=exp "in" e=exp                        {`LetIn ($loc, i, v, e)}
  | "let" "《" i=exp_bid "/" t=typ_bid "》" "=" v=exp "in" e=exp {`UnpackIn ($loc, t, i, v, e)}

//

program:
  | e=exp EOF                                         {e}

typ_exp:
  | t=typ EOF                                         {t}
