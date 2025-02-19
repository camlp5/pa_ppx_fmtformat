(**pp -syntax camlp5o *)

type paren_kind_t =
  PAREN
| PAREN_BAR
| BRACKET
| BRACKET_BAR
| BRACE
| BRACE_BAR
| ANGLE
| ANGLE_BAR

type token =
  Text of string
| Interpolate of paren_kind_t * string * string option
| EOF

type template_t = ((int * int) * token) list
