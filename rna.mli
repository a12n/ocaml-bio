module Nt : sig
  type nt = A | C | G | U
  include Seq.Elt_sig with type t = nt
end

include module type of Seq.Make (Nt)

module Codon : sig
  type t = Nt.t * Nt.t * Nt.t
end

module Gen_code : sig
  module type Sig = sig
    val rev_translate : Aa.t -> Codon.t list
    val start_codons : Codon.t list
    val stop_codons : Codon.t list
    val translate : Codon.t -> Aa.t option
  end

  (** Standard. *)
  module Std : Sig

  (** Vertebrate Mitochondrial. *)
  module Vert_mt : Sig
end
