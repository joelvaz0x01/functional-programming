(** @autor: Joel Vaz (a48906) *)

(** Declaration of numbers and operations with the module Zarith *)
let zero, one, two, three, six =
  (Z.zero, Z.one, Z.of_int 2, Z.of_int 3, Z.of_int 6)

let ( +! ), ( -! ), ( *! ), ( /! ) = (Z.add, Z.sub, Z.mul, Z.div)

exception Invalid_value
(** Creates an exception for invalid values *)

(**
    Function of the sum of the first expression
    @param n : number of order of the expression
    @param exp : function first_exp
    @param count_exp : counter of the function first_exp
    @param acc : accumulator of the result of the sum
    @return result of the sum
*)
let rec sum_k_n k n exp count_exp acc =
  if k > n - 2 then acc (* Final result of the sum (stop condition) *)
  else
    (* Declaration of the expressions of the sum *)
    let (inter_exp1, _), (inter_exp2, _) =
      (exp k count_exp, exp (n - k - 1) count_exp)
    in
    (* Calculation of the expression of the sum *)
    let sum_exp = inter_exp1 *! inter_exp2 in
    sum_k_n (k + 1) n exp count_exp (acc +! sum_exp)

(**
    Function of the first expression (without memoization)
    n is not necessary to be in the type Z.t (module Zarith)
    @param n : number of order of the expression
    @param count : count the number of times the function is executed
    @return result of the first expression
*)
let rec first_exp n count =
  (* Increment counter *)
  incr count;
  match n with
  | 0 -> (one, count) (* When 'n' is 0 return 1. Output : 1, count *)
  | 1 -> (two, count) (* When 'n' is 1 return 2. Output : 2, count *)
  | _ ->
      (* When 'n' is not 0 and 1 *)
      (* Result of the sum of the first expression *)
      let inter_exp, _ = first_exp (n - 1) count in
      (* Output : (result of the first expression, count) *)
      ((three *! inter_exp) +! sum_k_n 1 n first_exp count zero, count)

(**
    Function of the second expression (without memoization)
    n is not necessary to be in the type Z.t (module Zarith)
    @param n : number of order of the expression
    @param count : count the number of times the function is executed
    @return result of the second expression
*)
let rec second_exp n count =
  (* Increment counter *)
  incr count;
  match n with
  | 0 -> (one, count) (* When 'n' is 0 return 1. Output : 1, count *)
  | 1 -> (two, count) (* When 'n' is 1 return 2. Output : 2, count *)
  | _ ->
      (* When 'n' is not 0 and 1 *)
      (* Result of the sum of the second expression *)
      let (inter_exp1, _), (inter_exp2, _) =
        (second_exp (n - 1) count, second_exp (n - 2) count)
      in
      (* Output : (result of the second expression, count) *)
      ( (((six *! Z.of_int n) -! three) *! inter_exp1 /! Z.of_int (n + 1))
        -! (Z.of_int (n - 2) *! inter_exp2 /! Z.of_int (n + 1)),
        count )

(**
    Function of the second expression (with memoization)
    n is not necessary to be in the type Z.t (module Zarith)
    @param n : number of order of the expression
    @param h_table : hash table
    @return result of the second expression
*)
let rec memo_apply n h_table =
  (* Verify if the result of 'n' exists in the hash table *)
  if Hashtbl.mem h_table n then Hashtbl.find h_table n
  else
    match n with
    | 0 ->
        (* if 'n' is 0 return 1 and add the result to the hash table *)
        Hashtbl.add h_table n one;
        one
    | 1 ->
        (* if 'n' is 1 return 2 and add the result to the hash table *)
        Hashtbl.add h_table n two;
        two
    | _ ->
        (* if 'n' is not 0 and 1 *)
        (* Second expression *)
        let exp =
          ((six *! Z.of_int n) -! three)
          *! memo_apply (n - 1) h_table
          /! Z.of_int (n + 1)
          -! (Z.of_int (n - 2) *! memo_apply (n - 2) h_table /! Z.of_int (n + 1))
        in
        (* Add the result to the hash table *)
        Hashtbl.add h_table n exp;
        exp

(** Main function *)
let () =
  try
    let a, b = Scanf.scanf "%d %d" (fun first second -> (first, second)) in
    (* Create a hash table with b+1 positions because 'b' starts at 0 *)
    let hash_table = Hashtbl.create (b + 1) in
    if a < 0 || a > 20 || b < 0 || b > 10000 then raise Invalid_value
    else
      let (exp1, first_counter), (exp2, second_counter), fast_exp =
        (first_exp a (ref 0), second_exp a (ref 0), memo_apply b hash_table)
      in
      Printf.printf "%a %d\n%a %d\n%a\n" Z.output exp1 !first_counter Z.output
        exp2 !second_counter Z.output fast_exp
  with _ -> raise Invalid_value
