let pp_dna f dna =
  Format.(pp_print_char f '"';
          pp_print_string f (Bio.Dna.to_string dna);
          pp_print_char f '"')
;;

#install_printer pp_dna;;
