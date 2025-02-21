(**pp -syntax camlp5o -package pa_ppx.import,pa_ppx.deriving_plugins.std *)
open OUnit2
open Pa_ppx_testutils
open Pa_ppx_fmtformat
open Pa_fmtformat

let printer s = s

type paren_kind_t = [%import: Pa_ppx_fmtformat.Interp_tokens.paren_kind_t] [@@deriving show { with_path = false }]
type pos_t = [%import: Pa_ppx_fmtformat.Interp_tokens.pos_t] [@@deriving show { with_path = false }]
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
      ; ((4, 5), (Interpolate (PAREN, ((4, 5), "b"), None)))
      ; ((6, 8), (Text " c"))
      ]
      (template_of_string Ploc.dummy {|a $(b) c|})
  ; assert_equal ~printer:show_template_t
      [
        ((0, 2), (Text "a "))
      ; ((5, 6), (Interpolate (PAREN_BAR, ((5, 6), "b"), None)))
      ; ((8, 10), (Text " c"))
      ]
      (template_of_string Ploc.dummy {|a $(|b|) c|})
  ; assert_equal ~printer:show_template_t
      [
        ((0, 2), (Text "a "))
      ; ((5, 8), (Interpolate (PAREN_BAR, ((5, 6), "b"), (Some ((7, 8), "x")))))
      ; ((10, 12), (Text " c"))
      ]
      (template_of_string Ploc.dummy {|a $(|b|x|) c|})
  ; assert_equal ~printer:show_template_t
      [
        ((0, 2), (Text "a "))
      ; ((5, 12), (Interpolate (PAREN_BAR, ((6, 7), "b"), (Some ((10, 11), "x")))))
      ; ((14, 16), (Text " c"))
      ]
      (template_of_string Ploc.dummy {|a $(| b | x |) c|})
  ; assert_equal ~printer:show_template_t
      [
        ((0, 2), (Text "a "))
      ; ((4, 11), (Interpolate (PAREN, ((5, 6), "b"), (Some ((9, 10), "x")))))
      ; ((12, 14), (Text " c"))
      ]
      (template_of_string Ploc.dummy {|a $( b | x ) c|})

let suite = "Test machinery" >::: [
      "lexer"   >:: test_lexer
    ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()

