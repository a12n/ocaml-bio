let pp_dna f dna = Format.fprintf f "\"%s\"" (Bio.Dna.to_string dna);;

let pp_dna_ambig f dna = Format.fprintf f "\"%s\"" (Bio.Dna.Ambig.to_string dna);;

#install_printer pp_dna;;
#install_printer pp_dna_ambig;;
