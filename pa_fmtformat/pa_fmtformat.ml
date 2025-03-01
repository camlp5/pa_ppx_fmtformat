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
    if eof = snd tok then
      List.rev acc
    else lrec (tok::acc)
  in
  lrec []

let template_of_string loc s =
  try
    let lb = Lexing.from_string s in
    list_of_lexer_eof EOF Interp_lexer.token lb
  with e ->
        Fmt.(raise_failwithf loc "pa_fmtformat: while analyzing \"%s\" got exception: %a\n" s exn e)

let format_string_of_template ~forfmt t =
  let l = t |> List.map (function
    EOF -> assert false
  | Text s -> s |> [%subst "%" / "%%" / g i pcre2] |> [%subst {|\$\$|} / "$" / g i pcre2]
  | Interpolate(_, _, None) -> "%s"
  | Interpolate(_, _, Some (_, fmt)) ->
     assert ("" <> fmt) ;
     if String.get fmt 0 = '%' then
       fmt
     else if forfmt then "%a" else "%a"
                 ) in
  String.concat "" l

let do_parse_expr str = (Grammar.Entry.parse Pcaml.expr_eoi) (Stream.of_string str) ;;

let eval_anti entry loc typ str : Ploc.t * MLast.expr =
  let loc =
    let sh =
      if typ = "" then String.length "$"
      else
        String.length "$" + String.length typ + String.length ":"
    in
    let len = String.length str in
    Ploc.sub loc sh len
  in
  let r =
    try
      Ploc.call_with Plexer.force_antiquot_loc false
        do_parse_expr str
    with
    Ploc.Exc(loc1, exc) ->
        let shift = Ploc.first_pos loc in
        let loc =
          Ploc.make_loc (Ploc.file_name loc)
            (Ploc.line_nb loc + Ploc.line_nb loc1 - 1)
            (if Ploc.line_nb loc1 = 1 then Ploc.bol_pos loc
             else shift + Ploc.bol_pos loc1)
            (shift + Ploc.first_pos loc1,
             shift + Ploc.last_pos loc1) ""
          in
          raise (Ploc.Exc(loc, exc))
  in
  (loc, r)

let reloc_to_subloc ~enclosed subloc =
  Ploc.(sub enclosed (first_pos subloc) (last_pos subloc))

let parse_expr loc str =
  let (_,e) = eval_anti Pcaml.expr_eoi loc "" str in
  let shift = 0 in
  Reloc.expr (fun subloc -> reloc_to_subloc ~enclosed:loc subloc) shift e

let exprs_of_template loc t =
  t |> List.concat_map (fun (pos, tok) ->
    let mkloc (bp, ep) = Ploc.sub loc bp (ep-bp) in
    match tok with
      EOF -> assert false
    | Text s -> []
    | Interpolate(_,(argpos,arg),None) ->
       [parse_expr (mkloc argpos) arg]
    | Interpolate(_,(argpos,arg),Some (_,fmt)) when String.get fmt 0 = '%' ->
       [parse_expr (mkloc argpos) arg]
    | Interpolate(_,(argpos,arg),Some (fmtpos,fmt)) ->
       [parse_expr (mkloc fmtpos) fmt; parse_expr (mkloc argpos) arg]
         )

let fmt_str_expr_of_template loc t =
  let fmt_string = t |> List.map snd |> format_string_of_template ~forfmt:true in
  let el = exprs_of_template loc t in
  let e = Expr.applist <:expr< str $str:fmt_string$ >> el in
   <:expr< Fmt.($e$) >>

let rewrite_fmt_str arg = function
  <:expr< [%fmt_str $exp:e$ ] >> ->
   let (loc, s) = match e with <:expr< $locstr:(loc,s)$ >> -> (loc, Pcaml.unvala s) in
   fmt_str_expr_of_template loc (s |> Scanf.unescaped |> template_of_string loc)

| _ -> assert false

let fmt_pf_expr_of_template loc t =
  let fmt_string = t |> List.map snd |> format_string_of_template ~forfmt:true in
  let el = exprs_of_template loc t in
  let e = Expr.applist <:expr< pf pps $str:fmt_string$ >> el in
   <:expr< fun pps () -> Fmt.($e$) >>

let rewrite_fmt_pf arg = function
  <:expr< [%fmt_pf $exp:e$ ] >> ->
   let (loc, s) = match e with <:expr< $locstr:(loc,s)$ >> -> (loc, Pcaml.unvala s) in
   fmt_pf_expr_of_template loc (s |> Scanf.unescaped |> template_of_string loc)

| _ -> assert false

let sprintf_expr_of_template loc t =
  let fmt_string = t |> List.map snd |> format_string_of_template ~forfmt:false in
  let el = exprs_of_template loc t in
  let e = Expr.applist <:expr< sprintf $str:fmt_string$ >> el in
   <:expr< Pa_ppx_fmtformat_runtime.Sfmt.($e$) >>

let rewrite_sprintf arg = function
  <:expr< [%sprintf $exp:e$ ] >> ->
   let (loc, s) = match e with <:expr< $locstr:(loc,s)$ >> -> (loc, Pcaml.unvala s) in
   sprintf_expr_of_template loc (s |> Scanf.unescaped |> template_of_string loc)

| _ -> assert false

let fprintf_expr_of_template loc t =
  let fmt_string = t |> List.map snd |> format_string_of_template ~forfmt:false in
  let el = exprs_of_template loc t in
  let e = Expr.applist <:expr< fprintf pps $str:fmt_string$ >> el in
   <:expr< fun pps -> Pa_ppx_fmtformat_runtime.Pfmt.($e$) >>

let rewrite_fprintf arg = function
  <:expr< [%fprintf $exp:e$ ] >> ->
   let (loc, s) = match e with <:expr< $locstr:(loc,s)$ >> -> (loc, Pcaml.unvala s) in
   fprintf_expr_of_template loc (s |> Scanf.unescaped |> template_of_string loc)

| _ -> assert false

let printf_expr_of_template loc t =
  let fmt_string = t |> List.map snd |> format_string_of_template ~forfmt:false in
  let el = exprs_of_template loc t in
  let e = Expr.applist <:expr< printf $str:fmt_string$ >> el in
   <:expr< Pa_ppx_fmtformat_runtime.Pfmt.($e$) >>

let rewrite_printf arg = function
  <:expr< [%printf $exp:e$ ] >> ->
   let (loc, s) = match e with <:expr< $locstr:(loc,s)$ >> -> (loc, Pcaml.unvala s) in
   printf_expr_of_template loc (s |> Scanf.unescaped |> template_of_string loc)

| _ -> assert false

let sub_sprintf_expr_of_template loc t =
  let fmt_string = t |> List.map snd |> format_string_of_template ~forfmt:false in
  let el = exprs_of_template loc t in
  let e = Expr.applist <:expr< sprintf $str:fmt_string$ >> el in
   <:expr< fun () () -> Pa_ppx_fmtformat_runtime.Sfmt.($e$) >>

let rewrite_sub_sprintf arg = function
  <:expr< [%sub_sprintf $exp:e$ ] >> ->
   let (loc, s) = match e with <:expr< $locstr:(loc,s)$ >> -> (loc, Pcaml.unvala s) in
   sub_sprintf_expr_of_template loc (s |> Scanf.unescaped |> template_of_string loc)

| _ -> assert false

let sub_fprintf_expr_of_template loc t =
  let fmt_string = t |> List.map snd |> format_string_of_template ~forfmt:false in
  let el = exprs_of_template loc t in
  let e = Expr.applist <:expr< fprintf pps $str:fmt_string$ >> el in
   <:expr< fun pps () -> Pa_ppx_fmtformat_runtime.Pfmt.($e$) >>

let rewrite_sub_fprintf arg = function
  <:expr< [%sub_fprintf $exp:e$ ] >> ->
   let (loc, s) = match e with <:expr< $locstr:(loc,s)$ >> -> (loc, Pcaml.unvala s) in
   sub_fprintf_expr_of_template loc (s |> Scanf.unescaped |> template_of_string loc)

| _ -> assert false

let install () = 
let ef = EF.mk () in 
let ef = EF.{ (ef) with
            expr = extfun ef.expr with [
    <:expr:< [%fmt_str $str:_$ ] >> as z ->
    fun arg fallback ->
      Some (rewrite_fmt_str arg z)
  | <:expr:< [%fmt_pf $str:_$ ] >> as z ->
    fun arg fallback ->
      Some (rewrite_fmt_pf arg z)
  | <:expr:< [%sprintf $str:_$ ] >> as z ->
    fun arg fallback ->
      Some (rewrite_sprintf arg z)
  | <:expr:< [%fprintf $str:_$ ] >> as z ->
    fun arg fallback ->
      Some (rewrite_fprintf arg z)
  | <:expr:< [%printf $str:_$ ] >> as z ->
    fun arg fallback ->
      Some (rewrite_printf arg z)
  | <:expr:< [%sub_sprintf $str:_$ ] >> as z ->
    fun arg fallback ->
      Some (rewrite_sub_sprintf arg z)
  | <:expr:< [%sub_fprintf $str:_$ ] >> as z ->
    fun arg fallback ->
      Some (rewrite_sub_fprintf arg z)
  ] } in

  Pa_passthru.(install { name = "pa_fmtformat"; ef =  ef ; pass = None ; before = [] ; after = [] })
;;

install();;
