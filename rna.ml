module Nt = struct
  type nt = A | C | G | U and t = nt

  let n = 4

  let of_char = function
    | 'A' | 'a' -> A
    | 'C' | 'c' -> C
    | 'G' | 'g' -> G
    | 'U' | 'u' -> U
    | _ -> invalid_arg "Rna.Nt.of_char"

  let of_int = function
    (* As in 2bit for DNA *)
    | 0b10 -> A
    | 0b01 -> C
    | 0b11 -> G
    | 0b00 -> U
    | _ -> invalid_arg "Rna.Nt.of_int"

  let to_char = function
    | A -> 'A'
    | C -> 'C'
    | G -> 'G'
    | U -> 'U'

  let to_int = function
    (* As in 2bit for DNA *)
    | A -> 0b10
    | C -> 0b01
    | G -> 0b11
    | U -> 0b00
end

include Seq.Make (Nt)

module Codon = struct
  type t = Nt.t * Nt.t * Nt.t
end

module Gen_code = struct
  module type Sig = sig
    val rev_translate : Aa.t -> Codon.t list
    val stop_codons : Codon.t list
    val translate : Codon.t -> Aa.t option
  end

  let find_stop_codons translate =
    let nts = Nt.[A; C; G; U] in
    Batteries.List.(
      n_cartesian_product [nts; nts; nts] |>
      filter_map (
        function [base1; base2; base3] ->
          let codon = base1, base2, base3 in
          (match translate codon with
           | Some _aa -> None
           | None -> Some codon)
               | _other -> None
      ))
end
