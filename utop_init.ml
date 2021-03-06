#use "topfind";;
#require "batteries";;

open Batteries;;

#load "_build/bio.cma";;

open Bio;;

let pp_dna = IO.to_f_printer Dna.print_quoted;;
let pp_dna_ambig = IO.to_f_printer Dna.Ambig.print_quoted;;
let pp_prot = IO.to_f_printer Prot.print_quoted;;
let pp_rna = IO.to_f_printer Rna.print_quoted;;

#install_printer pp_dna;;
#install_printer pp_dna_ambig;;
#install_printer pp_prot;;
#install_printer pp_rna;;
