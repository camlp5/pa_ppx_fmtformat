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

}

rule token = parse
  | ([^ '$'] | "$$") +
      { (text_location lexbuf, Text (Lexing.lexeme lexbuf)) }
  | "$(|" { interp_body_thru_bar_rparen lexbuf }
  | "$(" { interp_body_thru_rparen lexbuf }
  | "$[|" { interp_body_thru_bar_rbracket lexbuf }
  | "$[" { interp_body_thru_rbracket lexbuf }
  | "${|" { interp_body_thru_bar_rbrace lexbuf }
  | "${" { interp_body_thru_rbrace lexbuf }
  | "$<|" { interp_body_thru_bar_rangle lexbuf }
  | "$<" { interp_body_thru_rangle lexbuf }

  | '$' (eof | [^ '(' '[' '{' '<']) { error lexbuf "Invalid template: Cannot have bare '$' -- use instead '$$'" }

  | eof { (text_location lexbuf, EOF) }

and interp_body_thru_bar_rparen = parse
    (([^ '|' ')'])+ as text) ("|" as sep) (([^ '|'] | "|" [^ ')'])+ as fmt) "|)"
    { let (full_pos,txt_pos,fmt_pos) = interp_locations ~text ~sep:true ~fmt ~subtract_end:2 lexbuf
      in (full_pos, Interpolate(PAREN_BAR, stripws (txt_pos, text), Some (stripws (fmt_pos,fmt)))) }
  | (([^ '|' ')'])+ as text) "|)"
    { let (full_pos,txt_pos,fmt_pos) = interp_locations ~text ~sep:false ~fmt:"" ~subtract_end:2 lexbuf
      in (full_pos, Interpolate(PAREN_BAR, stripws (txt_pos, text), None)) }
  | _ { error lexbuf "Invalid template: Unclosed '$(|'" }
  | eof { error lexbuf "Invalid template: Unclosed '$(|'" }

and interp_body_thru_rparen = parse
    (([^ '|' ')'])+ as text) ("|" as sep) (([^ ')'])+ as fmt) ")"
    { let (full_pos,txt_pos,fmt_pos) = interp_locations ~text ~sep:true ~fmt ~subtract_end:1 lexbuf
      in (full_pos, Interpolate(PAREN, stripws (txt_pos, text), Some (stripws (fmt_pos,fmt)))) }
  | (([^ '|' ')'])+ as text) ")"
    { let (full_pos,txt_pos,fmt_pos) = interp_locations ~text ~sep:false ~fmt:"" ~subtract_end:1 lexbuf
      in (full_pos, Interpolate(PAREN, stripws (txt_pos, text), None)) }
  | _ { error lexbuf "Invalid template: Unclosed '$('" }
  | eof { error lexbuf "Invalid template: Unclosed '$('" }

and interp_body_thru_bar_rbracket = parse
    (([^ '|' ']'])+ as text) ("|" as sep) (([^ '|'] | "|" [^ ']'])+ as fmt) "|]"
    { let (full_pos,txt_pos,fmt_pos) = interp_locations ~text ~sep:true ~fmt ~subtract_end:2 lexbuf
      in (full_pos, Interpolate(BRACKET_BAR, stripws (txt_pos, text), Some (stripws (fmt_pos,fmt)))) }
  | (([^ '|' ']'])+ as text) "|]"
    { let (full_pos,txt_pos,fmt_pos) = interp_locations ~text ~sep:false ~fmt:"" ~subtract_end:2 lexbuf
      in (full_pos, Interpolate(BRACKET_BAR, stripws (txt_pos, text), None)) }
  | _ { error lexbuf "Invalid template: Unclosed '$[|'" }
  | eof { error lexbuf "Invalid template: Unclosed '$[|'" }

and interp_body_thru_rbracket = parse
    (([^ '|' ']'])+ as text) ("|" as sep) (([^ ']'])+ as fmt) "]"
    { let (full_pos,txt_pos,fmt_pos) = interp_locations ~text ~sep:true ~fmt ~subtract_end:1 lexbuf
      in (full_pos, Interpolate(BRACKET, stripws (txt_pos, text), Some (stripws (fmt_pos,fmt)))) }
  | (([^ '|' ']'])+ as text) "]"
    { let (full_pos,txt_pos,fmt_pos) = interp_locations ~text ~sep:false ~fmt:"" ~subtract_end:1 lexbuf
      in (full_pos, Interpolate(BRACKET, stripws (txt_pos, text), None)) }
  | _ { error lexbuf "Invalid template: Unclosed '$['" }
  | eof { error lexbuf "Invalid template: Unclosed '$['" }

and interp_body_thru_bar_rbrace = parse
    (([^ '|' '}'])+ as text) ("|" as sep) (([^ '|'] | "|" [^ '}'])+ as fmt) "|}"
    { let (full_pos,txt_pos,fmt_pos) = interp_locations ~text ~sep:true ~fmt ~subtract_end:2 lexbuf
      in (full_pos, Interpolate(BRACE_BAR, stripws (txt_pos, text), Some (stripws (fmt_pos,fmt)))) }
  | (([^ '|' '}'])+ as text) "|}"
    { let (full_pos,txt_pos,fmt_pos) = interp_locations ~text ~sep:false ~fmt:"" ~subtract_end:2 lexbuf
      in (full_pos, Interpolate(BRACE_BAR, stripws (txt_pos, text), None)) }
  | _ { error lexbuf "Invalid template: Unclosed '${|'" }
  | eof { error lexbuf "Invalid template: Unclosed '${|'" }

and interp_body_thru_rbrace = parse
    (([^ '|' '}'])+ as text) ("|" as sep) (([^ '}'])+ as fmt) "}"
    { let (full_pos,txt_pos,fmt_pos) = interp_locations ~text ~sep:true ~fmt ~subtract_end:1 lexbuf
      in (full_pos, Interpolate(BRACE, stripws (txt_pos, text), Some (stripws (fmt_pos,fmt)))) }
  | (([^ '|' '}'])+ as text) "}"
    { let (full_pos,txt_pos,fmt_pos) = interp_locations ~text ~sep:false ~fmt:"" ~subtract_end:1 lexbuf
      in (full_pos, Interpolate(BRACE, stripws (txt_pos, text), None)) }
  | _ { error lexbuf "Invalid template: Unclosed '${'" }
  | eof { error lexbuf "Invalid template: Unclosed '${'" }

and interp_body_thru_bar_rangle = parse
    (([^ '|' '>'])+ as text) ("|" as sep) (([^ '|'] | "|" [^ '>'])+ as fmt) "|>"
    { let (full_pos,txt_pos,fmt_pos) = interp_locations ~text ~sep:true ~fmt ~subtract_end:2 lexbuf
      in (full_pos, Interpolate(ANGLE_BAR, stripws (txt_pos, text), Some (stripws (fmt_pos,fmt)))) }
  | (([^ '|' '>'])+ as text) "|>"
    { let (full_pos,txt_pos,fmt_pos) = interp_locations ~text ~sep:false ~fmt:"" ~subtract_end:2 lexbuf
      in (full_pos, Interpolate(ANGLE_BAR, stripws (txt_pos, text), None)) }
  | _ { error lexbuf "Invalid template: Unclosed '$<|'" }
  | eof { error lexbuf "Invalid template: Unclosed '$<|'" }

and interp_body_thru_rangle = parse
    (([^ '|' '>'])+ as text) ("|" as sep) (([^ '>'])+ as fmt) ">"
    { let (full_pos,txt_pos,fmt_pos) = interp_locations ~text ~sep:true ~fmt ~subtract_end:1 lexbuf
      in (full_pos, Interpolate(ANGLE, stripws (txt_pos, text), Some (stripws (fmt_pos,fmt)))) }
  | (([^ '|' '>'])+ as text) ">"
    { let (full_pos,txt_pos,fmt_pos) = interp_locations ~text ~sep:false ~fmt:"" ~subtract_end:1 lexbuf
      in (full_pos, Interpolate(ANGLE, stripws (txt_pos, text), None)) }
  | _ { error lexbuf "Invalid template: Unclosed '$<'" }
  | eof { error lexbuf "Invalid template: Unclosed '$<'" }
