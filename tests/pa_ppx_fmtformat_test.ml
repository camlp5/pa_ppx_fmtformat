(**pp -syntax camlp5o *)
open OUnit2

let printer s = s

let test_str ctxt =
  ()
  ; assert_equal "" [%fmt_str ""]
  ; (let c = "argle" in assert_equal ~printer "a b c argle d e f" {%fmt_str|a b c $(c) d e f|})
  ; (assert_equal ~printer "a b c argle d e f" {%fmt_str|a b c $("argle") d e f|})
  ; (assert_equal ~printer "a b c argle d e f" {%fmt_str|a b c $("argle"|string) d e f|})
  ; (assert_equal ~printer {|a b c "argle" d e f|} {%fmt_str|a b c $("argle"|Dump.string) d e f|})
  ; (let c = "argle" in assert_equal ~printer "a b c argle d e f" {%fmt_str|a b c $(|c|) d e f|})
  ; (let c = "argle" in assert_equal ~printer "a b c argle d e f" {%fmt_str|a b c $(|c|string|) d e f|})
  ; (assert_equal ~printer "a b c argle d e f" {%fmt_str|a b c $(|"argle"|string|) d e f|})

let pp_to_buffer f =
  let buf = Format.stdbuf in
  Buffer.clear buf ;
  f Format.str_formatter ;
  Format.flush_str_formatter ()


let test_pf ctxt =
  ()
  ; assert_equal "" (pp_to_buffer [%fmt_pf ""])
  ; (assert_equal ~printer "a b c argle d e f" (pp_to_buffer {%fmt_pf|a b c $("argle") d e f|}))

let suite = "Test pa_ppx_fmtformat" >::: [
      "str"   >:: test_str
    ; "pf"   >:: test_pf
    ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()

