(**pp -syntax camlp5o -package pa_ppx_fmtformat *)
open OUnit2
open Pa_ppx_testutils
open Pa_ppx_fmtformat

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
  ; (assert_equal ~printer "0b10_1010" [%fmt_str "${42|of_to_string Pp_binary_ints.Int.to_string}"])


let pp_to_buffer f =
  let buf = Format.stdbuf in
  Buffer.clear buf ;
  f Format.str_formatter () ;
  Format.flush_str_formatter ()


let test_pf ctxt =
  ()
  ; assert_equal "" (pp_to_buffer [%fmt_pf ""])
  ; (assert_equal ~printer "a b c argle d e f" (pp_to_buffer {%fmt_pf|a b c $("argle") d e f|}))
  ; assert_equal ~printer:(fun x -> "<<"^x^">>") "(foo, bar)\n" (pp_to_buffer [%fmt_pf {|${("foo", "bar")| parens (pair ~sep:(const string ", ") string string) }@.|}])

let test_dollar ctxt =
  ()
  ; assert_equal ~printer " $ " {%fmt_str| $$ |}
  ; assert_equal ~printer " $ % " {%fmt_str| $$ % |}
  ; Testutil.assert_raises_exn_pattern "Invalid template" (fun _ -> Pa_fmtformat.template_of_string Ploc.dummy {| $ |})
  ; Testutil.assert_raises_exn_pattern "Invalid template" (fun _ -> Pa_fmtformat.template_of_string Ploc.dummy {| $( |})
  ; Testutil.assert_raises_exn_pattern "Invalid template" (fun _ -> Pa_fmtformat.template_of_string Ploc.dummy {| $(|})
  ; Testutil.assert_raises_exn_pattern "Invalid template" (fun _ -> Pa_fmtformat.template_of_string Ploc.dummy {| $(|foo|})
  ; Testutil.assert_raises_exn_pattern "Invalid template" (fun _ -> Pa_fmtformat.template_of_string Ploc.dummy {| $(|")"|string|) |})

let test_location_accuracy ctxt =
  let loc = Ploc.make_unlined (10, 10) in
  ()
  ; assert_equal ~printer " $ " {%fmt_str| $$ |}

let test_sprintf ctxt =
  ()
  ; assert_equal "foo" {%sprintf|foo|}
  ; assert_equal "foo42" {%sprintf|foo$(42|%d)|}
  ; assert_equal "foo42(abc)" {%sprintf|foo$(42|int)$("abc"|parens string)|}

let suite = "Test pa_ppx_fmtformat" >::: [
      "str"   >:: test_str
    ; "pf"   >:: test_pf
    ; "dollar"   >:: test_dollar
    ; "location accuracy"   >:: test_location_accuracy
    ; "test sprintf" >:: test_sprintf
    ]

let _ = 
if not !Sys.interactive then
  run_test_tt_main suite
else ()

