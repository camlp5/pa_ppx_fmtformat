(**pp -syntax camlp5o *)
(* camlp5o *)
(* runtime.ml,v *)

open Pa_ppx_base
open Ppxutil

module Sfmt = struct
  type 'a t = unit -> 'a -> string
  let sprintf = Printf.sprintf

  let int () n = Printf.sprintf "%d" n
  let string () s = s
  let parens pp1 () x = Printf.sprintf "(%a)" pp1 x
end

module Pfmt = struct
  type 'a t = out_channel -> 'a -> unit
  let fprintf = Printf.fprintf
  let printf = Printf.printf
  let stdout = Stdlib.stdout
  let stderr = Stdlib.stderr
  let int oc n = fprintf oc "%d" n
  let string oc n = output_string oc n
  let parens pp1 oc x = fprintf oc "(%a)" pp1 x
end

