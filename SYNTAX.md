# Fωμ toy type checker &mdash; Syntax summary

Below is an approximation of the detailed
[grammar](src/main/FomParser/Grammar.mly) of the language:

```g4
 kind : '(' kind ')'
      | '*'
      | kind '→' kind

  typ : '(' typ ')'
      | id
      | 'int'
      | 'bool'
      | typ '→' typ
      | '{' ( label ':' typ ',' )* '}'
      | typ typ
      | 'λ' id ( ':' kind )? '.' typ
      | 'μ' id ( ':' kind )? '.' typ
      | '∀' id ( ':' kind )? '.' typ

  exp : '(' exp ')'
      | id
      | int
      | 'true' | 'false'
      | '{' ( label '=' exp ',' )* '}'
      | exp '.' label
      | exp exp
      | uop exp
      | exp bop exp
      | 'let' id '=' exp 'in' exp
      | 'let' 'type' id '=' typ 'in' exp
      | 'if' exp 'then' exp 'else' exp
      | 'λ' id ':' typ '.' exp
      | 'μ' id ':' typ '.' exp
      | 'Λ' id ( ':' kind )? '.' exp

  uop : '¬' | '+' | '-'

  bop : '∨' | '∧'
      | ( '=' | '≠' ) '[' typ ']'
      | '>' | '≥' | '<' | '≤'
      | '+' | '-'
      | '*' | '/' | '%'
```

Alternative tokens:

| Symbolic | Mnemonic                  |
| -------: | :------------------------ |
|      `.` | `=>`                      |
|      `¬` | `!`                       |
|      `Λ` | `gen`                     |
|      `λ` | `fun`                     |
|      `μ` | `rec`                     |
|      `→` | `->`                      |
|      `∀` | `forall`                  |
|      `∧` | `&&`                      |
|      `∨` | <code>&#124;&#124;</code> |
|      `≤` | `<=`                      |
|      `≥` | `>=`                      |
