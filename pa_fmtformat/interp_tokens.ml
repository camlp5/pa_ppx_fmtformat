(**pp -syntax camlp5o -package pa_ppx_regexp *)

type paren_kind_t =
  PAREN
| PAREN_BAR
| BRACKET
| BRACKET_BAR
| BRACE
| BRACE_BAR
| ANGLE
| ANGLE_BAR

type pos_t = int * int

type token =
  Text of string
| Interpolate of paren_kind_t * (pos_t * string) * (pos_t * string) option
| EOF

type template_t = ((int * int) * token) list

let strip_leading_ws ((bp,ep), s) =
  let (wstxt,txt) = [%match "^([ \n\t]*)([^ \n\t].*)$" /pcre2 exc strings (!1,!2)] s
  in ((bp + String.length wstxt,ep), txt)

let strip_trailing_ws ((bp,ep),s) =
  let (txt,wstxt) = [%match "^(.*[^ \n\t])([ \n\t]*)$" /pcre2 exc strings (!1,!2)] s
  in ((bp,ep - String.length wstxt), txt)

let stripws s = s |> strip_leading_ws |> strip_trailing_ws

let interp_locations ~text ~sep ~fmt ~subtract_end lexbuf =
  let bp = Lexing.lexeme_start lexbuf in
  let ep = Lexing.lexeme_end lexbuf in
  let full_pos = (bp, ep - subtract_end) in

  let textlen = String.length text in
  let seplen = if sep then 1 else 0 in
  let fmtlen = String.length fmt in

  let text_bp = bp in
  let text_ep = bp + textlen in
  let text_pos = (text_bp, text_ep) in

  let fmt_bp = bp + textlen + seplen in
  let fmt_ep = bp + textlen + seplen + fmtlen in
  let fmt_pos = (fmt_bp, fmt_ep) in

  (full_pos, text_pos, fmt_pos)

let text_location lexbuf =
  (Lexing.lexeme_start lexbuf, Lexing.lexeme_end lexbuf)
