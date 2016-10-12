let pp_dna f dna = Format.fprintf f "\"%s\"" (Bio.Dna.to_string dna);;

#install_printer pp_dna;;
