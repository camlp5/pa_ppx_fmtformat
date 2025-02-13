(**pp -syntax camlp5o *)

type token =
  Text of string
| Interpolate of string * string * string option
| EOF
