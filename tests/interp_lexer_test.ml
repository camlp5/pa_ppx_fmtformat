(**pp -syntax camlp5o -package pa_ppx.import,pa_ppx.deriving_plugins.std *)
open OUnit2
open Pa_ppx_testutils
open Pa_ppx_fmtformat
open Pa_fmtformat

let printer s = s

type paren_kind_t = [%import: Pa_ppx_fmtformat.Interp_tokens.paren_kind_t] [@@deriving show { with_path = false }]
type token = [%import: Pa_ppx_fmtformat.Interp_tokens.token] [@@deriving show { with_path = false }]
type template_t = [%import: Pa_ppx_fmtformat.Interp_tokens.template_t] [@@deriving show { with_path = false }]

open Interp_tokens

let test_lexer ctxt =
  ()
  ; assert_equal "" ""
  ; assert_equal [] (template_of_string Ploc.dummy {||})
  ; assert_equal ~printer:show_template_t
      [((0, 5), (Text "a b c"))]
      (template_of_string Ploc.dummy {|a b c|})
  ; assert_equal ~printer:show_template_t
      [
        ((0, 2), (Text "a "))
      ; ((4, 5), (Interpolate (PAREN, "b", None)))
      ; ((6, 8), (Text " c"))
      ]
      (template_of_string Ploc.dummy {|a $(b) c|})
  ; assert_equal ~printer:show_template_t
      [
        ((0, 2), (Text "a "))
      ; ((5, 6), (Interpolate (PAREN_BAR, "b", None)))
      ; ((8, 10), (Text " c"))
      ]
      (template_of_string Ploc.dummy {|a $(|b|) c|})

let suite = "Test interp_lexer" >::: [
      "lexer"   >:: test_lexer
    ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()

