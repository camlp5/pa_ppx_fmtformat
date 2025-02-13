(**pp -syntax camlp5o *)
(* camlp5o *)
(* pa_string.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Pa_ppx_base
open Pa_ppx_utils
open Pa_passthru
open Ppxutil
open Interp_tokens

type ff_fmt_t = Printf of string | Fmt of string

type ff_interpolated_expression_t =
  { exp : string ; fmt : ff_fmt_t }

type template_element_t = Text of string | Interpolate of ff_interpolated_expression_t
type template_t = template_element_t list

let list_of_lexer_eof eof lexfun lexbuf =
  let rec lrec acc =
    let tok = lexfun lexbuf in
    if eof = tok then
      List.rev acc
    else lrec (tok::acc)
  in
  lrec []

let template_of_string s =
  let lb = Lexing.from_string s in
  list_of_lexer_eof EOF Interp_lexer.token lb

let rewrite_fmtformat arg = function
  <:expr:< [%fmt_str $str:s$ ] >> ->
   <:expr<  $str:s$ >>
| _ -> assert false

let install () = 
let ef = EF.mk () in 
let ef = EF.{ (ef) with
            expr = extfun ef.expr with [
    <:expr:< [%fmt_str $str:_$ ] >> as z ->
    fun arg fallback ->
      Some (rewrite_fmtformat arg z)
  ] } in

  Pa_passthru.(install { name = "pa_fmtformat"; ef =  ef ; pass = None ; before = [] ; after = [] })
;;

install();;
