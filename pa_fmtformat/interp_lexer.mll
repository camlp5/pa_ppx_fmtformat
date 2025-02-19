(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* The lexer definition *)

{
open Lexing
open Interp_tokens

exception Error of string * Location.t

let error lexbuf e = raise (Error(e, Location.curr lexbuf))
let error_loc loc e = raise (Error(e, loc))

let strip_leading_ws s = Pcre2.(replace ~pat:"^[ \n\t]+" ~itempl:(subst "") s)
let strip_trailing_ws s = Pcre2.(replace ~pat:"[ \n\t]+$" ~itempl:(subst "") s)
let stripws s = s |> strip_leading_ws |> strip_trailing_ws

let location ~subtract_end lexbuf = (Lexing.lexeme_start lexbuf, (Lexing.lexeme_end lexbuf) - subtract_end)
}

rule token = parse
  | ([^ '$'] | "$$") +
      { (location ~subtract_end:0 lexbuf, Text (Lexing.lexeme lexbuf)) }
  | "$(|" { interp_body_thru_bar_rparen lexbuf }
  | "$(" { interp_body_thru_rparen lexbuf }
  | "$[|" { interp_body_thru_bar_rbracket lexbuf }
  | "$[" { interp_body_thru_rbracket lexbuf }
  | "${|" { interp_body_thru_bar_rbrace lexbuf }
  | "${" { interp_body_thru_rbrace lexbuf }
  | "$<|" { interp_body_thru_bar_rangle lexbuf }
  | "$<" { interp_body_thru_rangle lexbuf }

  | '$' (eof | [^ '(' '[' '{' '<']) { error lexbuf "Invalid template: Cannot have bare '$' -- use instead '$$'" }

  | eof { (location ~subtract_end:0 lexbuf, EOF) }

and interp_body_thru_bar_rparen = parse
    (([^ '|' ')'])+ as text) "|" (([^ '|'] | "|" [^ ')'])+ as fmt) "|)"
    { (location ~subtract_end:2 lexbuf, Interpolate(PAREN_BAR, stripws text, Some (stripws fmt))) }
  | (([^ '|' ')'])+ as text) "|)"
    { (location ~subtract_end:2 lexbuf, Interpolate(PAREN_BAR, stripws text, None)) }
  | _ { error lexbuf "Invalid template: Unclosed '$(|'" }
  | eof { error lexbuf "Invalid template: Unclosed '$(|'" }

and interp_body_thru_rparen = parse
    (([^ '|' ')'])+ as text) "|" (([^ ')'])+ as fmt) ")"
    { (location ~subtract_end:1 lexbuf, Interpolate(PAREN, stripws text, Some (stripws fmt))) }
  | (([^ '|' ')'])+ as text) ")"
    { (location ~subtract_end:1 lexbuf, Interpolate(PAREN, stripws text, None)) }
  | _ { error lexbuf "Invalid template: Unclosed '$('" }
  | eof { error lexbuf "Invalid template: Unclosed '$('" }

and interp_body_thru_bar_rbracket = parse
    (([^ '|' ']'])+ as text) "|" (([^ '|'] | "|" [^ ']'])+ as fmt) "|]"
    { (location ~subtract_end:2 lexbuf, Interpolate(PAREN_BAR, stripws text, Some (stripws fmt))) }
  | (([^ '|' ']'])+ as text) "|]"
    { (location ~subtract_end:2 lexbuf, Interpolate(PAREN_BAR, stripws text, None)) }
  | _ { error lexbuf "Invalid template: Unclosed '$[|'" }
  | eof { error lexbuf "Invalid template: Unclosed '$[|'" }

and interp_body_thru_rbracket = parse
    (([^ '|' ']'])+ as text) "|" (([^ ']'])+ as fmt) "]"
    { (location ~subtract_end:1 lexbuf, Interpolate(PAREN, stripws text, Some (stripws fmt))) }
  | (([^ '|' ']'])+ as text) "]"
    { (location ~subtract_end:1 lexbuf, Interpolate(PAREN, stripws text, None)) }
  | _ { error lexbuf "Invalid template: Unclosed '$['" }
  | eof { error lexbuf "Invalid template: Unclosed '$['" }

and interp_body_thru_bar_rbrace = parse
    (([^ '|' '}'])+ as text) "|" (([^ '|'] | "|" [^ '}'])+ as fmt) "|}"
    { (location ~subtract_end:2 lexbuf, Interpolate(PAREN_BAR, stripws text, Some (stripws fmt))) }
  | (([^ '|' '}'])+ as text) "|}"
    { (location ~subtract_end:2 lexbuf, Interpolate(PAREN_BAR, stripws text, None)) }
  | _ { error lexbuf "Invalid template: Unclosed '${|'" }
  | eof { error lexbuf "Invalid template: Unclosed '${|'" }

and interp_body_thru_rbrace = parse
    (([^ '|' '}'])+ as text) "|" (([^ '}'])+ as fmt) "}"
    { (location ~subtract_end:1 lexbuf, Interpolate(PAREN, stripws text, Some (stripws fmt))) }
  | (([^ '|' '}'])+ as text) "}"
    { (location ~subtract_end:1 lexbuf, Interpolate(PAREN, stripws text, None)) }
  | _ { error lexbuf "Invalid template: Unclosed '${'" }
  | eof { error lexbuf "Invalid template: Unclosed '${'" }

and interp_body_thru_bar_rangle = parse
    (([^ '|' '>'])+ as text) "|" (([^ '|'] | "|" [^ '>'])+ as fmt) "|>"
    { (location ~subtract_end:2 lexbuf, Interpolate(PAREN_BAR, stripws text, Some (stripws fmt))) }
  | (([^ '|' '>'])+ as text) "|>"
    { (location ~subtract_end:2 lexbuf, Interpolate(PAREN_BAR, stripws text, None)) }
  | _ { error lexbuf "Invalid template: Unclosed '$<|'" }
  | eof { error lexbuf "Invalid template: Unclosed '$<|'" }

and interp_body_thru_rangle = parse
    (([^ '|' '>'])+ as text) "|" (([^ '>'])+ as fmt) ">"
    { (location ~subtract_end:1 lexbuf, Interpolate(PAREN, stripws text, Some (stripws fmt))) }
  | (([^ '|' '>'])+ as text) ">"
    { (location ~subtract_end:1 lexbuf, Interpolate(PAREN, stripws text, None)) }
  | _ { error lexbuf "Invalid template: Unclosed '$<'" }
  | eof { error lexbuf "Invalid template: Unclosed '$<'" }
