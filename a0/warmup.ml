(** @author Yixin Cui (yc2494) *)

(** [valid_date y m d] checks whether [y] [m] [d] is a valid date *)
let valid_date y m d =
  if y <= 0 then false
  else 
    let day_in_month = [
      ("Jan", 31);
      ("Feb", 28);
      ("Mar", 31);
      ("Apr", 30);
      ("May", 31);
      ("Jun", 30);
      ("Jul", 31);
      ("Aug", 31);
      ("Sep", 30);
      ("Oct", 31);
      ("Nov", 30);
      ("Dec", 31);
    ] in
    let rec is_valid_month month month_list =
      match month_list with
      | [] -> -1
      | (mon, day)::tail -> if month = mon then day
        else is_valid_month month tail in
    let is_valid_day day =
      let is_leap_year year =
        year mod 4 = 0 && year mod 100 != 0 || year mod 400 = 0 in
      if is_leap_year y && m = "Feb" then day <= is_valid_month m day_in_month + 1 && day >= 1
      else day <= is_valid_month m day_in_month && day >= 1 in
    is_valid_day d

(** [syr n] is the number of operations needed to reduce [n] to 1*)  
let rec syr n =
  if n = 1 then 0
  else if n mod 2 = 0 then 1 + syr(n / 2)
  else syr(3 * n + 1) + 1

(** [nacci n k] is the [n] step Fibonacci sequence with [k] elements *)
let nacci n k =
  let rec sum lst step =
    if step <= 0 then 0
    else 
      match lst with
      | [] -> 0 
      | h::t -> h + sum t (step - 1) in
  let rec helper count total lst step =
    if count >= total then lst
    else helper (count + 1) total (sum lst step::lst) step in
  if k < 1 || n < 1 then []
  else if k = 1 then [1]
  else if k = 2 then [1; 1]
  else List.rev (helper 2 k [1; 1] n)
    

(* Tests *)
let () = assert (valid_date 2020 "Feb" 29)
let () = assert (not (valid_date 2010 "Jun" 50))
let () = assert (valid_date 2000 "Feb" 29)
let () = assert (not (valid_date 1000 "Feb" 29))
let () = assert (valid_date 1234 "Dec" 31)
let () = assert (not (valid_date 2222 "Sep" 31))
let () = assert (not (valid_date (-455) "Oct" 29))
let () = assert (not (valid_date 2010 "Jun" 0))
let () = assert (not (valid_date 2019 "Abc" 1))

let () = assert (syr 1 = 0)
let () = assert (syr 2 = 1)
let () = assert (syr 10 = 6)

let () = assert (nacci 1 6 = [1;1;1;1;1;1])
let () = assert (nacci 2 6 = [1;1;2;3;5;8])
let () = assert (nacci 3 6 = [1;1;2;4;7;13])
let () = assert (nacci 5 1 = [1])
let () = assert (nacci 6 2 = [1;1])
let () = assert (nacci 7 3 = [1;1;2])
let () = assert (nacci 1 0 = [])


let hours_worked = 2