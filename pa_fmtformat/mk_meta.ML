#include "version.inc"

let top = try Sys.getenv "TOP" with Not_found -> "" ;;
let destdir = ref (top ^ "/local-install") ;;

Arg.(parse [
         "-destdir", (Arg.String (fun s -> destdir := s)),
         ("<path>    Set the destination directory (default: " ^
	    !destdir ^ ")")
       ]
       (fun _ -> failwith "mk_meta: no anonymous args supported")
     "mk_meta -destdir <dir>")
;;

Fmt.(pf stdout {|
# Specifications for the "pa_ppx_fmtformat" preprocessor:
requires = "pcre2,camlp5,fmt,pa_ppx.base,pa_ppx.utils,pa_ppx_fmtformat_runtime,camlp5.parser_quotations,pa_ppx_regexp.runtime"
version = "%s"
description = "pa_ppx_fmtformat: pa_ppx_fmtformat rewriter"

# For linking
package "link" (
requires = "camlp5,fmt,pa_ppx.base.link,pa_ppx.utils.link,camlp5.parser_quotations.link,pa_ppx_regexp.runtime.link"
archive(byte) = "pa_ppx_fmtformat.cma"
archive(native) = "pa_ppx_fmtformat.cmxa"
)

# For the toploop:
archive(byte,toploop) = "pa_ppx_fmtformat.cma"

  # For the preprocessor itself:
  requires(syntax,preprocessor) = "camlp5,fmt,pa_ppx.base,pa_ppx.utils,camlp5.parser_quotations,pa_ppx_regexp.runtime"
  archive(syntax,preprocessor,-native) = "pa_ppx_fmtformat.cma"
  archive(syntax,preprocessor,native) = "pa_ppx_fmtformat.cmxa"

|} version)
