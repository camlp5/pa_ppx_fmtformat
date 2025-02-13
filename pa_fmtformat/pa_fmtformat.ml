(**pp -syntax camlp5o -package pa_ppx_regexp *)
(* camlp5o *)
(* pa_string.ml,v *)
(* Copyright (c) INRIA 2007-2017 *)

open Pa_ppx_base
open Pa_ppx_utils
open Pa_passthru
open Ppxutil
open Interp_tokens

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

let format_string_of_template t =
  let l = t |> List.map (function
    EOF -> assert false
  | Text s -> [%subst "%" / "%%" / g i pcre2] s
  | Interpolate(_, _, None) -> "%s"
  | Interpolate(_, _, Some fmt) ->
     assert ("" <> fmt) ;
     if String.get fmt 0 = '%' then
       fmt
     else "%a"
                 ) in
  String.concat "" l

let parse_expr str =
  let cs = Stream.of_string str in
  Grammar.Entry.parse Pcaml.expr cs

let exprs_of_template t =
  t |> List.concat_map (function
    EOF -> assert false
  | Text s -> []
  | Interpolate(_,arg,None) ->
     [parse_expr arg]
  | Interpolate(_,arg,Some fmt) when String.get fmt 0 = '%' ->
     [parse_expr arg]
  | Interpolate(_,arg,Some fmt) ->
     [parse_expr arg; parse_expr fmt]
         )

let fmt_str_expr_of_template loc t =
  let fmt_string = format_string_of_template t in
  let el = exprs_of_template t in
  Expr.applist <:expr< Fmt.(str $str:fmt_string$) >> el

let rewrite_fmtformat arg = function
  <:expr:< [%fmt_str $str:s$ ] >> ->
   fmt_str_expr_of_template loc (template_of_string s)
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
