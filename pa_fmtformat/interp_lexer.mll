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
}

rule token = parse
  | ([^ '$'] | "$$") +
      { Text (Lexing.lexeme lexbuf) }
  | "$(|" { interp_body_thru_bar_rparen lexbuf }

  | eof { EOF }

and interp_body_thru_bar_rparen = parse
    (([^ '|'])+ as text) "|" (([^ '|'] | "|" [^ ')'])+ as fmt) "|)" { Interpolate("(|", text, Some fmt) }
  | (([^ '|'])+ as text) "|)" { Interpolate("(|", text, None) }
