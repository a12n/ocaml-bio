let pp f fmt s = Format.fprintf fmt "\"%s\"" (f s);;

let pp_dna = pp Bio.Dna.to_string;;

let pp_dna_ambig = pp Bio.Dna.Ambig.to_string;;

let pp_prot = pp Bio.Prot.to_string;;

let pp_rna = pp Bio.Rna.to_string;;

#install_printer pp_dna;;
#install_printer pp_dna_ambig;;
#install_printer pp_prot;;
#install_printer pp_rna;;
