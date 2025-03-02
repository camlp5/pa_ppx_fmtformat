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
  let nop _ () = ""
  let list ?sep pp_elt pps l =
    let sep = match sep with Some f -> f () () | None -> "" in
    match l with
      [] -> ""
    | [h] -> pp_elt () h
    | l -> String.concat sep (List.map (pp_elt ()) l)
  let const pp1 x () _ = pp1 () x
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
  let nop _ () = ()
  let iter ?sep:(pp_sep = nop) iter pp_elt ppf v =
    let is_first = ref true in
    let pp_elt v =
      if !is_first then (is_first := false) else pp_sep ppf ();
      pp_elt ppf v
    in
    iter pp_elt v
  let list ?sep pp_elt = iter ?sep List.iter pp_elt
let const pp_v v ppf _ = pp_v ppf v
end

