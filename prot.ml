open BatPervasives

include Seq.Make (Aa)

(*$= edit_dist
  (edit_dist (of_string "") (of_string "MMM")) 3
  (edit_dist (of_string "MMM") (of_string "")) 3
  (edit_dist (of_string "MMM") (of_string "MMM")) 0
  (edit_dist (of_string "PLEASANTLY") (of_string "MEANLY")) 5
*)

let mass_aux f = fold_left (fun ans aa -> ans +. f aa) 0.0

let avg_mass = mass_aux Aa.avg_mass

let mass = mass_aux Aa.mass
