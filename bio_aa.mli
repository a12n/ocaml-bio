type aa = A | R | N | D | C | Q | E | G | H | I
        | L | K | M | F | P | S | T | W | Y | V
        (* | O | U *)
include Bio_seq.Elt_sig with type t = aa

val abbr : t -> string
val avg_mass : t -> float
val from_mass : ?epsilon:float -> float -> t option
val mass : t -> float
val name : t -> string

val blosum62 : t -> t -> int
val pam250 : t -> t -> int
