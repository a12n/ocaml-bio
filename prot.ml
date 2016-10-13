open BatPervasives

include Seq.Make (Aa)

let mass_aux f = fold_left (fun ans aa -> ans +. f aa) 0.0

let avg_mass = mass_aux Aa.avg_mass

let mass = mass_aux Aa.mass
