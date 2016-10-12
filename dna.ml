open BatPervasives

module type Nt_sig = sig
  include Seq.Elt_sig
  val comp : t -> t
end

module Nt = struct
  type nt = A | C | G | T and t = nt

  let comp = function
    | A -> T
    | C -> G
    | G -> C
    | T -> A

  let n = 4

  let of_char = function
    | 'A' | 'a' -> A
    | 'C' | 'c' -> C
    | 'G' | 'g' -> G
    | 'T' | 't' -> T
    | _ -> invalid_arg "Dna.Nt.of_char"

  let of_int = function
    (* As in 2bit *)
    | 0b10 -> A
    | 0b01 -> C
    | 0b11 -> G
    | 0b00 -> T
    | _ -> invalid_arg "Dna.Nt.of_int"

  let to_char = function
    | A -> 'A'
    | C -> 'C'
    | G -> 'G'
    | T -> 'T'

  let to_int = function
    (* As in 2bit *)
    | A -> 0b10
    | C -> 0b01
    | G -> 0b11
    | T -> 0b00
end

include Seq.Make (Nt)

(*$= get
  (get (of_string "gattaca") 0) Nt.G
  (get (of_string "gattaca") 1) Nt.A
  (get (of_string "gattaca") 6) Nt.A
*)

(*$= is_empty
  (is_empty (of_string "")) true
  (is_empty (of_string "a")) false
*)

(*$= length
  (length (of_string "")) 0
  (length (of_string "a")) 1
  (length (of_string "ac")) 2
*)

(*$= rev
  (rev (of_string "")) (of_string "")
  (rev (of_string "atg")) (of_string "gta")
*)

(*$= count
  (count Nt.A (of_string "")) 0
  (count Nt.C (of_string "")) 0
  (count Nt.G (of_string "")) 0
  (count Nt.T (of_string "")) 0
  (count Nt.A (of_string "gattaca")) 3
  (count Nt.C (of_string "gattaca")) 1
  (count Nt.G (of_string "gattaca")) 1
  (count Nt.T (of_string "gattaca")) 2
*)

(*$= hamm_dist
  (hamm_dist (of_string "") (of_string "")) 0
  (hamm_dist (of_string "a") (of_string "a")) 0
  (hamm_dist (of_string "a") (of_string "c")) 1
  (hamm_dist (of_string "gattaca") (of_string "atgatga")) 6
*)

(*$T p_dist
  Batteries.Float.approx_equal (p_dist (of_string "") (of_string "")) 0.0
  Batteries.Float.approx_equal (p_dist (of_string "a") (of_string "a")) 0.0
  Batteries.Float.approx_equal (p_dist (of_string "a") (of_string "c")) 1.0
  Batteries.Float.approx_equal (p_dist (of_string "gattaca") (of_string "atgatga")) (6.0 /. 7.0)
*)

let comp = map Nt.comp

(*$= comp
  (comp (of_string "")) (of_string "")
  (comp (of_string "gattaca")) (of_string "ctaatgt")
*)

let gc_content s =
  match length s with
  | 0 -> invalid_arg "Dna.gc_content"
  | n -> float_of_int (count Nt.G s + count Nt.C s) /.
         float_of_int n

(*$T gc_content
  Batteries.Float.approx_equal (gc_content (of_string "att")) 0.0
  Batteries.Float.approx_equal (gc_content (of_string "ccg")) 1.0
  Batteries.Float.approx_equal (gc_content (of_string "gattaca")) (2.0 /. 7.0)
*)

let rev_comp = rev % comp

(*$= rev_comp
  (rev_comp (of_string "")) (of_string "")
  (rev_comp (of_string "gattaca")) (of_string "tgtaatc")
*)

let transcribe =
  Rna.of_enum % Batteries.Enum.map (
    function Nt.A -> Rna.Nt.A
           | Nt.C -> Rna.Nt.C
           | Nt.G -> Rna.Nt.G
           | Nt.T -> Rna.Nt.U
  ) % enum

module Ambig = struct
  module Nt = struct
    type nt = A | C | G | T | R | Y | S | W
            | K | M | B | D | H | V | N | Gap
    and t = nt

    let comp = function
      | A -> T
      | C -> G
      | G -> C
      | T -> A
      | R -> Y
      | Y -> R
      | S -> S
      | W -> W
      | K -> M
      | M -> K
      | B -> V
      | D -> H
      | H -> D
      | V -> B
      | N -> N
      | Gap -> Gap

    let n = 16

    let of_char = function
      | 'A' | 'a' -> A
      | 'C' | 'c' -> C
      | 'G' | 'g' -> G
      | 'T' | 't' -> T
      | 'R' | 'r' -> R
      | 'Y' | 'y' -> Y
      | 'S' | 's' -> S
      | 'W' | 'w' -> W
      | 'K' | 'k' -> K
      | 'M' | 'm' -> M
      | 'B' | 'b' -> B
      | 'D' | 'd' -> D
      | 'H' | 'h' -> H
      | 'V' | 'v' -> V
      | 'N' | 'n' -> N
      | '-' | '.' -> Gap
      | _ -> invalid_arg "Dna.Ambig.Nt.of_char"

    let of_int = function
      | 0b0000 -> Gap
      | 0b0001 -> A
      | 0b0010 -> C
      | 0b0100 -> G
      | 0b1000 -> T
      | 0b0101 -> R             (* A or G *)
      | 0b1010 -> Y             (* C or T *)
      | 0b0110 -> S             (* G or C *)
      | 0b1001 -> W             (* A or T *)
      | 0b1100 -> K             (* G or T *)
      | 0b0011 -> M             (* A or C *)
      | 0b1110 -> B             (* C or G or T *)
      | 0b1101 -> D             (* A or G or T *)
      | 0b1011 -> H             (* A or C or T *)
      | 0b0111 -> V             (* A or C or G *)
      | 0b1111 -> N             (* A or C or G or T *)
      | _ -> invalid_arg "Dna.Ambig.Nt.of_int"

    let to_char = function
      | A -> 'A'
      | C -> 'C'
      | G -> 'G'
      | T -> 'T'
      | R -> 'R'
      | Y -> 'Y'
      | S -> 'S'
      | W -> 'W'
      | K -> 'K'
      | M -> 'M'
      | B -> 'B'
      | D -> 'D'
      | H -> 'H'
      | V -> 'V'
      | N -> 'N'
      | Gap -> '-'

    let rec to_int = function
      | Gap -> 0b0000
      | A -> 0b0001
      | C -> 0b0010
      | G -> 0b0100
      | T -> 0b1000
      | R -> (to_int A) lor (to_int G)
      | Y -> (to_int C) lor (to_int T)
      | S -> (to_int G) lor (to_int C)
      | W -> (to_int A) lor (to_int T)
      | K -> (to_int G) lor (to_int T)
      | M -> (to_int A) lor (to_int C)
      | B -> (to_int C) lor (to_int G) lor (to_int T)
      | D -> (to_int A) lor (to_int G) lor (to_int T)
      | H -> (to_int A) lor (to_int C) lor (to_int T)
      | V -> (to_int A) lor (to_int C) lor (to_int G)
      | N -> (to_int A) lor (to_int C) lor (to_int G) lor (to_int T)
  end

  include Seq.Make (Nt)

  let comp = map Nt.comp

  let gc_content s =
    match length s with
    | 0 -> invalid_arg "Dna.Ambig.gc_content"
    | n -> float_of_int (count Nt.G s + count Nt.C s + count Nt.S s) /.
           float_of_int n

  let rev_comp = rev % comp
end
