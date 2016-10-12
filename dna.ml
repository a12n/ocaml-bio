module Nt = struct
  type t = A | C | G | T

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
    | 0 -> A
    | 1 -> C
    | 2 -> G
    | 3 -> T
    | _ -> invalid_arg "Dna.Nt.of_int"

  let to_char = function
    | A -> 'A'
    | C -> 'C'
    | G -> 'G'
    | T -> 'T'

  let to_int = function
    | A -> 0
    | C -> 1
    | G -> 2
    | T -> 3
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

let comp = map Nt.comp

(*$= comp
  (comp (of_string "")) (of_string "")
  (comp (of_string "gattaca")) (of_string "ctaatgt")
*)

let gc_content dna =
  assert (not (is_empty dna));
  float_of_int (count Nt.G dna + count Nt.C dna) /.
  float_of_int (length dna)

let rev_comp = Batteries.(rev % comp)

(*$= rev_comp
  (rev_comp (of_string "")) (of_string "")
  (rev_comp (of_string "gattaca")) (of_string "tgtaatc")
*)
