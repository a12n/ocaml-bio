type aa = A | R | N | D | C | Q | E | G | H | I | L
        | K | M | F | P | O | S | U | T | W | Y | V
include Seq.Elt_sig with type t = aa

val abbr : t -> string
val mass : t -> float
val name : t -> string
