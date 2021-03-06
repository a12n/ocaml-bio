module type Nt_sig = sig
  include Bio_seq.Elt_sig
  val comp : t -> t
end

module Nt : sig
  type nt = A | C | G | T
  include Nt_sig with type t = nt
end

include module type of Bio_seq.Make (Nt)

val comp : t -> t
val gc_content : t -> float
val gc_prob : float -> t -> float
val gc_skew : t -> int array
val num_trans : t -> t -> int
val num_transv : t -> t -> int
val rev_comp : t -> t
val transcribe : t -> Bio_rna.t

module Ambig : sig
  module Nt : sig
    type nt = A | C | G | T | R | Y | S | W
            | K | M | B | D | H | V | N | Gap
    include Nt_sig with type t = nt
  end

  include module type of Bio_seq.Make (Nt)

  val comp : t -> t
  val gc_content : t -> float
  val rev_comp : t -> t
end
