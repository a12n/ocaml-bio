open Batteries

let pp_dna = IO.to_f_printer Bio.Dna.print;;
let pp_dna_ambig = IO.to_f_printer Bio.Dna.Ambig.print;;
let pp_prot = IO.to_f_printer Bio.Prot.print;;
let pp_rna = IO.to_f_printer Bio.Rna.print;;

#install_printer pp_dna;;
#install_printer pp_dna_ambig;;
#install_printer pp_prot;;
#install_printer pp_rna;;
