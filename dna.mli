module type Nt_sig = sig
  include Seq.Elt_sig
  val comp : t -> t
end

module Nt : sig
  type nt = A | C | G | T
  include Nt_sig with type t = nt
end

include module type of Seq.Make (Nt)

val comp : t -> t
val gc_content : t -> float
val rev_comp : t -> t

module Ambig : sig
  module Nt : sig
    type nt = A | C | G | T | R | Y | S | W
            | K | M | B | D | H | V | N | Gap
    include Nt_sig with type t = nt
  end

  include module type of Seq.Make (Nt)

  val comp : t -> t
  val gc_content : t -> float
  val rev_comp : t -> t
end
