open Batteries

module type Nt_sig = sig
  include Bio_seq.Elt_sig
  val comp : t -> t
end

module Nt = struct
  (*$< Nt *)

  type nt = A | C | G | T and t = nt

  let comp = function
    | A -> T
    | C -> G
    | G -> C
    | T -> A

  let gc_prob gc = function
    | A | T -> (1.0 -. gc) /. 2.0
    | G | C -> gc /. 2.0

  let n = 4

  let of_char = function
    | 'A' | 'a' -> A
    | 'C' | 'c' -> C
    | 'G' | 'g' -> G
    | 'T' | 't' -> T
    | _ -> invalid_arg "Dna.Nt.of_char"

  let of_int = function
    | 0b00 -> A
    | 0b01 -> C
    | 0b10 -> G
    | 0b11 -> T
    | _ -> invalid_arg "Dna.Nt.of_int"

  let to_char = function
    | A -> 'A'
    | C -> 'C'
    | G -> 'G'
    | T -> 'T'

  let to_int = function
    | A -> 0b00
    | C -> 0b01
    | G -> 0b10
    | T -> 0b11

  (*$Q of_int
    (QCheck.oneofl [A; C; G; T]) (fun nt -> of_int (to_int nt) = nt)
  *)
  (*$Q of_char
    (QCheck.oneofl [A; C; G; T]) (fun nt -> of_char (to_char nt) = nt)
  *)

  let transcribe = function
    | A -> Bio_rna.Nt.A
    | C -> Bio_rna.Nt.C
    | G -> Bio_rna.Nt.G
    | T -> Bio_rna.Nt.U

  (*$>*)
end

include Bio_seq.Make (Nt)

(*$= delete
  (delete (of_string "") (of_string "")) (of_string "")
  (delete (of_string "ATG") (of_string "")) (of_string "ATG")
  (delete (of_string "GATTACA") (of_string "A")) (of_string "GTTC")
  (delete (of_string "") (of_string "ATG")) (of_string "")
  (delete (of_string "ATG") (of_string "ATG")) (of_string "")
*)

(*$= find_elt
  (Batteries.List.of_enum (find_elt (of_string "") Nt.A)) []
  (Batteries.List.of_enum (find_elt (of_string "atg") Nt.C)) []
  (Batteries.List.of_enum (find_elt (of_string "cccc") Nt.C)) [0; 1; 2; 3]
  (Batteries.List.of_enum (find_elt (of_string "gattaca") Nt.A)) [1; 4; 6]
  (Batteries.List.of_enum (find_elt (of_string "gattaca") Nt.C)) [5]
  (Batteries.List.of_enum (find_elt (of_string "gattaca") Nt.G)) [0]
  (Batteries.List.of_enum (find_elt (of_string "gattaca") Nt.T)) [2; 3]
  (Batteries.List.of_enum (find_elt (of_string "gattaca") ~first:2 Nt.T)) [2; 3]
  (Batteries.List.of_enum (find_elt (of_string "gattaca") ~first:3 Nt.T)) [3]
  (Batteries.List.of_enum (find_elt (of_string "gattaca") ~first:4 Nt.T)) []
*)

(*$T find_sub
  (Batteries.List.of_enum (find_sub (of_string "") (of_string "atg"))) = []
  (Batteries.List.of_enum (find_sub (of_string "gattaca") (of_string "ac"))) = [4]

  try ignore (Batteries.List.of_enum (find_sub (of_string "") (of_string ""))); \
  false with (Invalid_argument _) -> true

  try ignore (Batteries.List.of_enum (find_sub (of_string "gattaca") (of_string ""))); \
  false with (Invalid_argument _) -> true
*)

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

(*$= overlap_length
  (overlap_length (of_string "") (of_string "atg")) 0
  (overlap_length (of_string "aaa") (of_string "aaa")) 3
  (overlap_length (of_string "acgctg") (of_string "gctgcacg")) 4
  (overlap_length (of_string "gctgcacg") (of_string "acgctg")) 3
*)

(*$= rev
  (rev (of_string "")) (of_string "")
  (rev (of_string "atg")) (of_string "gta")
*)

(*$= int_of_kmer
  (int_of_kmer (of_string "aa")) 0
  (int_of_kmer (of_string "aaa")) 0
  (int_of_kmer (of_string "gg")) 10
  (int_of_kmer (of_string "")) 0
  (int_of_kmer (of_string "atg")) 14
*)

(*$= kmer_of_int
  (kmer_of_int ~k:1 0) (of_string "a")
  (kmer_of_int ~k:1 1) (of_string "c")
  (kmer_of_int ~k:1 2) (of_string "g")
  (kmer_of_int ~k:1 3) (of_string "t")
  (kmer_of_int ~k:2 0) (of_string "aa")
  (kmer_of_int ~k:2 1) (of_string "ac")
  (kmer_of_int ~k:2 2) (of_string "ag")
  (kmer_of_int ~k:2 14) (of_string "tg")
  (kmer_of_int ~k:2 10) (of_string "gg")
  (kmer_of_int ~k:2 15) (of_string "tt")
*)

(*$= kmers
  (Batteries.List.of_enum (kmers ~k:1 (of_string ""))) []
  (Batteries.List.of_enum (kmers ~k:1 (of_string "at"))) [of_string "a"; of_string "t"]
  (Batteries.List.of_enum (kmers ~k:2 (of_string "atg"))) [of_string "at"; of_string "tg"]
  (Batteries.List.of_enum (kmers ~k:3 (of_string "atg"))) [of_string "atg"]
  (Batteries.List.of_enum (kmers ~k:4 (of_string "atg"))) []
*)

(*$= kmer_compos
  (kmer_compos ~k:1 (of_string "")) [|0; 0; 0; 0|]
  (kmer_compos ~k:1 (of_string "atg")) [|1; 0; 1; 1|]
  (kmer_compos ~k:2 (of_string "gattaca")) [|0; 1; 0; 1; 1; 0; 0; 0; 1; 0; 0; 0; 1; 0; 0; 1|]
*)

(* TODO: compare to sorted result *)
(*$= kmer_clumps
  (Batteries.List.of_enum (kmer_clumps ~k:2 ~l:4 ~t:2 (of_string "AAAACGTCGAAAAA"))) [of_string "AA"]

  (Batteries.List.of_enum (kmer_clumps ~k:1 ~l:5 ~t:2 (of_string "ACGTACGT"))) \
  [of_string "C"; of_string "A"; of_string "G"; of_string "T"]

  (Batteries.List.of_enum (kmer_clumps ~k:3 ~l:25 ~t:3 (of_string "CCACGCGGTGTACGCTGCAAAAAGCCTTGCTGAATCAAATAAGGTTCCAGCACATCCTCAATGGTTTCACGTTCTTCGCCAATGGCTGCCGCCAGGTTATCCAGACCTACAGGTCCACCAAAGAACTTATCGATTACCGCCAGCAACAATTTGCGGTCCATATAATCGAAACCTTCAGCATCGACATTCAACATATCCAGCG"))) \
  [of_string "AAA"; of_string "CAG"; of_string "CCA"; of_string "CAT"; of_string "GCC"; of_string "TTC"]
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

(*$= edit_dist
  (edit_dist (of_string "") (of_string "")) 0
  (edit_dist (of_string "") (of_string "a")) 1
  (edit_dist (of_string "atg") (of_string "att")) 1
  (edit_dist (of_string "gattaca") (of_string "atg")) 5
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

(*$= lcs
  (lcs (of_string "") (of_string "")) (of_string "")
  (lcs (of_string "") (of_string "AAA")) (of_string "")
  (lcs (of_string "AAA") (of_string "")) (of_string "")
  (lcs (of_string "AACCTTGG") (of_string "ACACTGTGA")) (of_string "AACTTG")
*)

(*$= scs
  (scs (of_string "") (of_string "")) (of_string "")
  (scs (of_string "") (of_string "ATG")) (of_string "ATG")
  (scs (of_string "ATG") (of_string "")) (of_string "ATG")
  (scs (of_string "ATCTGAT") (of_string "TGCATA")) (of_string "ATGCATGAT")
*)

(*$< Pfm *)

(*$= consensus
  (consensus (from_list [of_string ""])) (of_string "")
  (consensus (from_list [of_string "ATG"])) (of_string "ATG")
  (consensus (from_list (List.map of_string \
  ["ATCCAGCT"; "GGGCAACT"; "ATGGATCT"; "AAGCAACC"; \
  "TTGGAACT"; "ATGCCATT"; "ATGGCACT"]))) (of_string "ATGCAACT")
*)

(*$>*)

(*$< Align *)

(*$= global
  (global (of_string "") (of_string "")) \
  (0, [])

  (global (of_string "gattaca") (of_string "atg")) \
  (-2, Nt.[`Delete G; `Match A; `Match T; \
  `Subst (T, G); `Delete A; `Delete C; `Delete A])
*)

(*$= local
  (local (of_string "") (of_string "")) \
  (0, [])

  (local (of_string "gattaca") (of_string "atg")) \
  (2, Nt.[`Match A; `Match T; `Subst (T, G)])
*)

(*$>*)

let comp = map Nt.comp

(*$= comp
  (comp (of_string "")) (of_string "")
  (comp (of_string "gattaca")) (of_string "ctaatgt")
*)

let gc_content s =
  match length s with
  | 0 -> invalid_arg "Dna.gc_content"
  | n -> float_of_int Nt.(count G s + count C s) /.
         float_of_int n

(*$T gc_content
  Batteries.Float.approx_equal (gc_content (of_string "att")) 0.0
  Batteries.Float.approx_equal (gc_content (of_string "ccg")) 1.0
  Batteries.Float.approx_equal (gc_content (of_string "gattaca")) (2.0 /. 7.0)
*)

let gc_prob gc s = Enum.fold ( *.) 1.0 (Enum.map (Nt.gc_prob gc) (enum s))

let gc_skew s =
  let n = length s in
  let ans = Array.make (n + 1) 0 in
  for i = 1 to n do
    match get s (i - 1) with
    | Nt.C -> ans.(i) <- ans.(i - 1) - 1
    | Nt.G -> ans.(i) <- ans.(i - 1) + 1
    | Nt.A | Nt.T -> ans.(i) <- ans.(i - 1)
  done;
  ans

let num_trans =
  fold_left2 Nt.(fun n si ti ->
      match si, ti with
      | A, G | G, A -> n + 1
      | C, T | T, C -> n + 1
      | _other -> n
    ) 0

(*$= num_trans
  (num_trans (of_string "") (of_string "")) 0
  (num_trans (of_string "AGTACGGG") (of_string "GGTACGAG")) 2
  (num_trans (of_string "ATG") (of_string "CGC")) 0
*)

let num_transv =
  fold_left2 Nt.(fun n si ti ->
      match si, ti with
      | A, C | C, A -> n + 1
      | A, T | T, A -> n + 1
      | G, C | C, G -> n + 1
      | G, T | T, G -> n + 1
      | _other -> n
    ) 0

(*$= num_transv
  (num_transv (of_string "") (of_string "")) 0
  (num_transv (of_string "ATG") (of_string "CGC")) 3
  (num_transv (of_string "GCAACGCA") (of_string "TTATCTGA")) 4
*)

let rev_comp = rev % comp

(*$= rev_comp
  (rev_comp (of_string "")) (of_string "")
  (rev_comp (of_string "gattaca")) (of_string "tgtaatc")
*)

let transcribe = Bio_rna.of_enum % Enum.map Nt.transcribe % enum

(*$= transcribe
  (transcribe (of_string "")) (Bio_rna.of_string "")
  (transcribe (of_string "gattaca")) (Bio_rna.of_string "gauuaca")
*)

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

  include Bio_seq.Make (Nt)

  let comp = map Nt.comp

  let gc_content s =
    match length s with
    | 0 -> invalid_arg "Dna.Ambig.gc_content"
    | n -> float_of_int (count Nt.G s + count Nt.C s + count Nt.S s) /.
           float_of_int n

  let rev_comp = rev % comp
end
