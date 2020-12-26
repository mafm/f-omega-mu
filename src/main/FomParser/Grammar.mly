%token <Bigint.t> NatLit
%token <bool> BoolLit
%token <string> Id
%token <string> Comment
%token ArrowRight "→"
%token Bool "bool"
%token BraceLhs "{"
%token BraceRhs "}"
%token BracketLhs "["
%token BracketRhs "]"
%token Colon ":"
%token Comma ","
%token Dot "."
%token EOF
%token Else "else"
%token Equal "="
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
%token Then "then"
%token Type "type"

%left "∨"
%left "∧"
%nonassoc "=" "≠" "]"
%nonassoc "<" "≤" "≥" ">"
%left "+" "-"
%left "*" "/" "%"

%start <Exp.t> program
%start <Typ.t> typ_exp

%{ open FomSyntax %}

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
  | "*"                                               {Kind.star $loc}
  | "(" k=kind ")"                                    {k}

kind:
  | k=kind_atom                                       {k}
  | d=kind_atom "→" c=kind                            {Kind.arrow $loc d c}

kind_annot:
  |                                                   {Kind.star $loc}
  | ":" k=kind                                        {k}

//

label:
  | i=Id                                              {Label.id $loc i}

//

lab_typ:
  | l=label ":" t=typ                                 {(l, t)}

typ_id:
  | i=Id                                              {Typ.Id.id $loc i}

typ_atom:
  | i=typ_id                                          {Typ.var $loc i}
  | "int"                                             {Typ.int $loc}
  | "bool"                                            {Typ.bool $loc}
  | "(" t=typ ")"                                     {t}
  | "{" fs=list_n(lab_typ, ",") "}"                   {Typ.product $loc fs}

typ_app:
  | t=typ_atom                                        {t}
  | f=typ_app x=typ_atom                              {Typ.app $loc f x}

typ_inf:
  | t=typ_app                                         {t}
  | d=typ_app "→" c=typ_inf                           {Typ.arrow $loc d c}

typ_bind(head):
  | head i=typ_id k=kind_annot "." t=typ              {Typ.lam $loc i k t}

typ:
  | t=typ_inf                                         {t}
  | t=typ_bind("μ")                                   {Typ.mu $loc t}
  | t=typ_bind("∀")                                   {Typ.for_all $loc t}
  | t=typ_bind("λ")                                   {t}

//

lab_exp:
  | l=label "=" e=exp                                 {(l, e)}

exp_id:
  | i=Id                                              {Exp.Id.id $loc i}

exp_atom:
  | i=exp_id                                          {Exp.var $loc i}
  | l=NatLit                                          {Exp.lit_nat $loc l}
  | l=BoolLit                                         {Exp.lit_bool $loc l}
  | "(" e=exp ")"                                     {e}
  | "{" fs=list_n(lab_exp, ",") "}"                   {Exp.product $loc fs}
  | e=exp_atom "." l=label                            {Exp.select $loc e l}

exp_app:
  | e=exp_atom                                        {e}
  | f=exp_app x=exp_atom                              {Exp.app $loc f x}
  | f=exp_app "[" x=typ "]"                           {Exp.inst $loc f x}

exp_inf:
  | e=exp_app                                         {e}
  | f=uop x=exp_app                                   {Exp.app $loc f x}
  | l=exp_inf o=bop r=exp_inf                         {Exp.bin_op $loc l o r}

%inline uop:
  | "¬"                                               {Exp.op_logical_not $loc}
  | "+"                                               {Exp.op_arith_plus $loc}
  | "-"                                               {Exp.op_arith_minus $loc}

%inline bop:
  | "∨"                                               {Exp.op_logical_or $loc}
  | "∧"                                               {Exp.op_logical_and $loc}

  | "=" "[" t=typ "]"                                 {Exp.op_eq $loc t}
  | "≠" "[" t=typ "]"                                 {Exp.op_eq_not $loc t}

  | ">"                                               {Exp.op_cmp_gt $loc}
  | "≥"                                               {Exp.op_cmp_gt_eq $loc}
  | "<"                                               {Exp.op_cmp_lt $loc}
  | "≤"                                               {Exp.op_cmp_lt_eq $loc}

  | "+"                                               {Exp.op_arith_add $loc}
  | "-"                                               {Exp.op_arith_sub $loc}

  | "*"                                               {Exp.op_arith_mul $loc}
  | "/"                                               {Exp.op_arith_div $loc}
  | "%"                                               {Exp.op_arith_rem $loc}

exp_bind(head):
  | head i=exp_id ":" t=typ "." e=exp                 {Exp.lam $loc i t e}

exp:
  | e=exp_inf                                         {e}
  | e=exp_bind("μ")                                   {Exp.fix $loc e}
  | e=exp_bind("λ")                                   {e}
  | "Λ" i=typ_id k=kind_annot "." e=exp               {Exp.gen $loc i k e}
  | "let" "type" i=typ_id "=" t=typ "in" e=exp        {Exp.let_typ_in i t e}
  | "let" i=exp_id "=" v=exp "in" e=exp               {Exp.let_in $loc i v e}
  | "if" c=exp "then" t=exp "else" e=exp              {Exp.if_else $loc c t e}

//

program:
  | e=exp EOF                                         {e}

typ_exp:
  | t=typ EOF                                         {t}
