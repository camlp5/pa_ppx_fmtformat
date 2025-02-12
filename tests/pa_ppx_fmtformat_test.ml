(**pp -syntax camlp5o *)
open OUnit2


let test_simple ctxt =
  ()
  ; assert_equal "" [%fmt_str ""]
  ; (let c = "argle" in assert_equal "a b c argle d e f" {%fmt_str|a b c $(c) d e f|})

let suite = "Test pa_ppx_fmtformat" >::: [
      "simple"   >:: test_simple
    ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()

