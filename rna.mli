module Nt : sig
  type nt = A | C | G | U
  include Seq.Elt_sig with type t = nt
end

include module type of Seq.Make (Nt)

module Codon : sig
  type t = Nt.t * Nt.t * Nt.t
end
