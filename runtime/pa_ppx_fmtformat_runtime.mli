
module Sfmt :
  sig
    type 'a t = unit -> 'a -> string
    val sprintf : ('a, unit, string) format -> 'a
    val int : int t
    val string : string t
    val parens : 'a t -> 'a t
    val nop : unit t
    val list : ?sep:unit t -> 'a t -> 'a list t
    val const : 'a t -> 'a -> 'b t
  end
module Pfmt :
  sig
    type 'a t = out_channel -> 'a -> unit
    val fprintf : out_channel -> ('a, out_channel, unit) format -> 'a
    val printf : ('a, out_channel, unit) format -> 'a

    val stdout : out_channel
    val stderr : out_channel
    val int : int t
    val string : string t
    val parens : 'a t -> 'a t
    val nop : unit t
    val list : ?sep:unit t -> 'a t -> 'a list t
    val const : 'a t -> 'a -> 'b t
  end
