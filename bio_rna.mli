open Batteries

module Nt : sig
  type nt = A | C | G | U
  include Bio_seq.Elt_sig with type t = nt
  val comp : t -> t
  val wobble : t -> t option
end

include module type of Bio_seq.Make (Nt)

module Codon : sig
  type t = Nt.t * Nt.t * Nt.t
end

module Rf : sig
  type t = Codon.t Enum.t
end

module Gen_code : sig
  module type Sig = sig
    val start_codons : Codon.t list
    val stop_codons : Codon.t list
    val translate : Codon.t -> Bio_aa.t option
  end

  (** Standard. Start codon list in [Std] genetic code contains only
      AUG. To extend it with alternative initiation codons UUG and CUG,
      new module must be derived from [Std]. *)
  module Std : sig
    include Sig
    val rev_translate : Bio_aa.t -> Codon.t list
  end

  (** Vertebrate Mitochondrial. *)
  module Vert_mt : Sig

  (** Yeast Mitochondrial. *)
  module Yeast_mt : Sig

  (** Mold, Protozoan, Coelenterate Mitochondrial & Mycoplasma/Spiroplasma. *)
  module Mold_mt : Sig
end

(** Returns reading frame 0 of RNA strand [s]. *)
val codons : t -> Rf.t

val orf : ?gen_code:(module Gen_code.Sig) -> Rf.t -> Rf.t option

(** Return all possible reading frames of RNA strand. *)
val rfs : t -> Rf.t Enum.t

(** Translate reading frame to a protein string according to specified
    genetic code ([Gen_code.Std] is used by default). *)
val translate : ?gen_code:(module Gen_code.Sig) -> Rf.t -> Bio_prot.t
