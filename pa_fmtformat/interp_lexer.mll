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

type error =
  | Illegal_character of char

exception Error of error * Location.t

let error lexbuf e = raise (Error(e, Location.curr lexbuf))
let error_loc loc e = raise (Error(e, loc))

let stripws s = Pcre2.(replace ~pat:"[ \n\t]" ~itempl:(subst "") s)
}

rule token = parse
  | ([^ '$'] | "$$") +
      { Text (Lexing.lexeme lexbuf) }
  | "$(|" { interp_body_thru_bar_rparen lexbuf }
  | "$(" { interp_body_thru_rparen lexbuf }
  | "$[|" { interp_body_thru_bar_rbracket lexbuf }
  | "$[" { interp_body_thru_rbracket lexbuf }
  | "${|" { interp_body_thru_bar_rbrace lexbuf }
  | "${" { interp_body_thru_rbrace lexbuf }
  | "$<|" { interp_body_thru_bar_rangle lexbuf }
  | "$<" { interp_body_thru_rangle lexbuf }

  | eof { EOF }

and interp_body_thru_bar_rparen = parse
    (([^ '|' ')'])+ as text) "|" (([^ '|'] | "|" [^ ')'])+ as fmt) "|)" { Interpolate(PAREN_BAR, stripws text, Some (stripws fmt)) }
  | (([^ '|' ')'])+ as text) "|)" { Interpolate(PAREN_BAR, stripws text, None) }

and interp_body_thru_rparen = parse
    (([^ '|' ')'])+ as text) "|" (([^ ')'])+ as fmt) ")" { Interpolate(PAREN, stripws text, Some (stripws fmt)) }
  | (([^ '|' ')'])+ as text) ")" { Interpolate(PAREN, stripws text, None) }

and interp_body_thru_bar_rbracket = parse
    (([^ '|' ']'])+ as text) "|" (([^ '|'] | "|" [^ ']'])+ as fmt) "|]" { Interpolate(PAREN_BAR, stripws text, Some (stripws fmt)) }
  | (([^ '|' ']'])+ as text) "|]" { Interpolate(PAREN_BAR, stripws text, None) }

and interp_body_thru_rbracket = parse
    (([^ '|' ']'])+ as text) "|" (([^ ']'])+ as fmt) "]" { Interpolate(PAREN, stripws text, Some (stripws fmt)) }
  | (([^ '|' ']'])+ as text) "]" { Interpolate(PAREN, stripws text, None) }

and interp_body_thru_bar_rbrace = parse
    (([^ '|' '}'])+ as text) "|" (([^ '|'] | "|" [^ '}'])+ as fmt) "|}" { Interpolate(PAREN_BAR, stripws text, Some (stripws fmt)) }
  | (([^ '|' '}'])+ as text) "|}" { Interpolate(PAREN_BAR, stripws text, None) }

and interp_body_thru_rbrace = parse
    (([^ '|' '}'])+ as text) "|" (([^ '}'])+ as fmt) "}" { Interpolate(PAREN, stripws text, Some (stripws fmt)) }
  | (([^ '|' '}'])+ as text) "}" { Interpolate(PAREN, stripws text, None) }

and interp_body_thru_bar_rangle = parse
    (([^ '|' '>'])+ as text) "|" (([^ '|'] | "|" [^ '>'])+ as fmt) "|>" { Interpolate(PAREN_BAR, stripws text, Some (stripws fmt)) }
  | (([^ '|' '>'])+ as text) "|>" { Interpolate(PAREN_BAR, stripws text, None) }

and interp_body_thru_rangle = parse
    (([^ '|' '>'])+ as text) "|" (([^ '>'])+ as fmt) ">" { Interpolate(PAREN, stripws text, Some (stripws fmt)) }
  | (([^ '|' '>'])+ as text) ">" { Interpolate(PAREN, stripws text, None) }
