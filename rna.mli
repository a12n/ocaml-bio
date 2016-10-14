module Nt : sig
  type nt = A | C | G | U
  include Seq.Elt_sig with type t = nt
end

include module type of Seq.Make (Nt)

module Codon : sig
  type t = Nt.t * Nt.t * Nt.t
end

module Rf : sig
  type t = Codon.t Batteries.Enum.t
end

module Gen_code : sig
  module type Sig = sig
    val start_codons : Codon.t list
    val stop_codons : Codon.t list
    val translate : Codon.t -> Aa.t option
  end

  (** Standard. *)
  module Std : sig
    include Sig
    val rev_translate : Aa.t -> Codon.t list
  end

  (** Vertebrate Mitochondrial. *)
  module Vert_mt : Sig

  (** Yeast Mitochondrial. *)
  module Yeast_mt : Sig

  (** Mold, Protozoan, Coelenterate Mitochondrial & Mycoplasma/Spiroplasma. *)
  module Mold_mt : Sig
end
