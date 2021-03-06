open Batteries

include Bio_seq.Make (Bio_aa)

(*$= edit_dist
  (edit_dist (of_string "") (of_string "MMM")) 3
  (edit_dist (of_string "MMM") (of_string "")) 3
  (edit_dist (of_string "MMM") (of_string "MMM")) 0
  (edit_dist (of_string "PLEASANTLY") (of_string "MEANLY")) 5
*)

(*$< Align *)

(*$= global
  (global (of_string "PRETTY") (of_string "PRTTEIN")) \
  (2, Bio_aa.[`Match P; `Match R; `Subst (E, T); `Match T; \
  `Insert E; `Subst (T, I); `Subst (Y, N)])

*)

(*$= local
  (let scoring = `Linear (-5), Bio_aa.pam250 in \
  (local ~scoring (of_string "MEANLYPRTEINSTRING") (of_string "PLEASANTLYEINSTEIN"))) \
  (23, Bio_aa.[`Match L; `Match Y; `Delete P; `Delete R; `Delete T; `Match E; \
  `Match I; `Match N; `Match S; `Match T; `Subst (R, E); `Match I; `Match N])
*)

(*$>*)

let mass_aux f = fold_left (fun ans aa -> ans +. f aa) 0.0

let avg_mass = mass_aux Bio_aa.avg_mass

let mass = mass_aux Bio_aa.mass
