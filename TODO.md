# Fωμ type checker &mdash; TODO

- Sandbox
  - highlight Fωμ based on grammar
- General
  - use alpha conversion in structural term equality and comparison
  - better parser and type checker error messages
  - more testing
  - pretty printer for expressions
  - take precendence and associativity properly into account in pretty printing
- Language
  - equality
  - existential types (missing proper error messages)
  - check recursive expressions for safety
  - more support for (JavaScript) primitives
    - bitwise operations on `int`
    - operations on `string`s
    - `BigInt`
    - `double`
    - `array` (as immutable?)
    - `println`
    - External calls:
      ```g4
      exp : 'primitive' '[' typ ']' string
      ```
- Documentation
  - non-trivial examples
- ToJs:
  - optimize:
    - hoist constant values (e.g. `rec`, `["nil": {}]}` -> `[nil, empty]`)
    - specialize recursive definitions
      - mutually recursive function definitions
      - product types
      - sum types
    - use destructuring e.g. for `λl.l case r` when `l` is not free in `r`
    - eliminate dead code when type is known to be uninhabited (e.g. empty sum)
  - features:
    - optional freezing of constants
    - source maps
