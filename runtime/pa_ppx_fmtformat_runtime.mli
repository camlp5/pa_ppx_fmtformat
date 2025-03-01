
module Sfmt :
  sig
    type 'a t = unit -> 'a -> string
    val sprintf : ('a, unit, string) format -> 'a
    val int : int t
    val string : string t
    val parens : 'a t -> 'a t
  end
module Pfmt :
  sig
    type 'a t = out_channel -> 'a -> unit
    val fprintf : out_channel -> ('a, out_channel, unit) format -> 'a
    val printf : ('a, out_channel, unit) format -> 'a
  end
