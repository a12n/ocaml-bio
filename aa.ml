type aa = A | R | N | D | C | Q | E | G | H | I | L
        | K | M | F | P | O | S | U | T | W | Y | V
and t = aa

let n = 22

let of_char = function
  | 'A' | 'a' -> A
  | 'R' | 'r' -> R
  | 'N' | 'n' -> N
  | 'D' | 'd' -> D
  | 'C' | 'c' -> C
  | 'Q' | 'q' -> Q
  | 'E' | 'e' -> E
  | 'G' | 'g' -> G
  | 'H' | 'h' -> H
  | 'I' | 'i' -> I
  | 'L' | 'l' -> L
  | 'K' | 'k' -> K
  | 'M' | 'm' -> M
  | 'F' | 'f' -> F
  | 'P' | 'p' -> P
  | 'O' | 'o' -> O
  | 'S' | 's' -> S
  | 'U' | 'u' -> U
  | 'T' | 't' -> T
  | 'W' | 'w' -> W
  | 'Y' | 'y' -> Y
  | 'V' | 'v' -> V
  | _other -> invalid_arg "Aa.of_char"

let of_int = function
  | 0 -> A
  | 1 -> R
  | 2 -> N
  | 3 -> D
  | 4 -> C
  | 5 -> Q
  | 6 -> E
  | 7 -> G
  | 8 -> H
  | 9 -> I
  | 10 -> L
  | 11 -> K
  | 12 -> M
  | 13 -> F
  | 14 -> P
  | 15 -> O
  | 16 -> S
  | 17 -> U
  | 18 -> T
  | 19 -> W
  | 20 -> Y
  | 21 -> V
  | _ -> invalid_arg "Aa.of_int"

let to_char = function
  | A -> 'A'
  | R -> 'R'
  | N -> 'N'
  | D -> 'D'
  | C -> 'C'
  | Q -> 'Q'
  | E -> 'E'
  | G -> 'G'
  | H -> 'H'
  | I -> 'I'
  | L -> 'L'
  | K -> 'K'
  | M -> 'M'
  | F -> 'F'
  | P -> 'P'
  | O -> 'O'
  | S -> 'S'
  | U -> 'U'
  | T -> 'T'
  | W -> 'W'
  | Y -> 'Y'
  | V -> 'V'

let to_int = function
  | A -> 0
  | R -> 1
  | N -> 2
  | D -> 3
  | C -> 4
  | Q -> 5
  | E -> 6
  | G -> 7
  | H -> 8
  | I -> 9
  | L -> 10
  | K -> 11
  | M -> 12
  | F -> 13
  | P -> 14
  | O -> 15
  | S -> 16
  | U -> 17
  | T -> 18
  | W -> 19
  | Y -> 20
  | V -> 21

let abbr = function
  | A -> "Ala"
  | R -> "Arg"
  | N -> "Asn"
  | D -> "Asp"
  | C -> "Cys"
  | Q -> "Gln"
  | E -> "Glu"
  | G -> "Gly"
  | H -> "His"
  | I -> "Ile"
  | L -> "Leu"
  | K -> "Lys"
  | M -> "Met"
  | F -> "Phe"
  | P -> "Pro"
  | O -> "Pyl"
  | S -> "Ser"
  | U -> "Sec"
  | T -> "Thr"
  | W -> "Trp"
  | Y -> "Tyr"
  | V -> "Val"

(** Average mass. Average of molecule's possible masses, taken over
    all isotopes of the molecule. *)
let avg_mass = function
  | A ->  71.0788
  | R -> 156.1875
  | N -> 114.1039
  | D -> 115.0886
  | C -> 103.1388
  | Q -> 128.1307
  | E -> 129.1155
  | G ->  57.0519
  | H -> 137.1411
  | I -> 113.1594
  | L -> 113.1594
  | K -> 128.1741
  | M -> 131.1986
  | F -> 147.1766
  | P ->  97.1167
  | O -> 237.3018
  | S ->  87.0782
  | U -> 150.0379
  | T -> 101.1051
  | W -> 186.2132
  | Y -> 163.1760
  | V ->  99.1326

(** Monoisotopic mass. The sum of the masses of the most common
    isotopes of the atoms making up a molecule. *)
let mass = function
  | A ->  71.03711
  | R -> 156.10111
  | N -> 114.04293
  | D -> 115.02694
  | C -> 103.00919
  | Q -> 128.05858
  | E -> 129.04259
  | G ->  57.02146
  | H -> 137.05891
  | I -> 113.08406
  | L -> 113.08406
  | K -> 128.09496
  | M -> 131.04049
  | F -> 147.06841
  | P ->  97.05276
  | O -> 237.14773
  | S ->  87.03203
  | U -> 150.95363
  | T -> 101.04768
  | W -> 186.07931
  | Y -> 163.06333
  | V ->  99.06841

let name = function
  | A -> "Alanine"
  | R -> "Arginine"
  | N -> "Asparagine"
  | D -> "Aspartic acid"
  | C -> "Cysteine"
  | Q -> "Glutamine"
  | E -> "Glutamic acid"
  | G -> "Glycine"
  | H -> "Histidine"
  | I -> "Isoleucine"
  | L -> "Leucine"
  | K -> "Lysine"
  | M -> "Methionine"
  | F -> "Phenylalanine"
  | P -> "Proline"
  | O -> "Pyrrolysine"
  | S -> "Serine"
  | U -> "Selenocysteine"
  | T -> "Threonine"
  | W -> "Tryptophan"
  | Y -> "Tyrosine"
  | V -> "Valine"