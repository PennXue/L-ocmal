(* Question 1 *)
(* TODO: Write your own tests for the fact function.
         See the provided tests for double, above, for how to write test cases.
         Remember that you should NOT test cases for n < 0.
*)
(* TODO: Correct these tests for the fact function. *)

let fact_tests = [
  (0, 1.);
  (1, 1.);
  (2, 2.);
  (5, 120.)
]

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let rec fact (n: int): float =
  match n with
  | 0 -> 1.
  | _ -> float_of_int n *. fact (n - 1)


(* TODO: Write your own tests for the binomial function.
         See the provided tests for fact, above, for how to write test cases.
         Remember that we assume that  n >= k >= 0; you should not write test cases where this assumption is violated.
*)
let binomial_tests = [
  (* Your test cases go here. Correct the incorrect test cases for the function. *)
  ((0, 0), 1.);
  ((1, 0), 1.);
  ((2, 0), 1.);
  ((10, 1), 10.);
  ((10, 2), 45.)
]

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let binomial (n: int) (k: int) =
  if n < 0
  then domain ()
  else (if k = n
        then 1. 
        else fact n /. (fact k *. fact (n-k)))


(* TODO: Write a good set of tests for ackerman. *)
let ackerman_tests = [
  ((0, 0), 1);
  ((1, 0), 2);
  ((0, 1), 2);
  ((2, 2), 7)
  (* Your test cases go here *)
]

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let ackerman (n, k) =
  if n < 0 || k < 0
  then domain ()
  else (let rec ack n k =
          match (n, k) with
          | (0, _) -> k + 1
          | (_, 0) -> ack (n - 1) 1
          | (_, _) -> ack (n - 1) (ack n (k - 1))
        in ack n k)


(* Question 2: is_prime *)

(* TODO: Write a good set of tests for is_prime. *)
let is_prime_tests = [ 
  (2, true);
  (3, true);
  (4, false);
  (6, false);
  (7, true) 
(* Your tests go here *) 
]

(* TODO: Correct this implementation so that it compiles and returns
         the correct answers.
*)
let is_prime n = 
  if n <= 1
  then domain() 
  else ( let rec pri n x : bool = 
           if x >= 1
           then (
             if x * x <= n && n mod x = 0 && x != 1
             then false
             else if x*x > n
             then true
             else pri n (x+1)
           ) 
           else domain()
         in pri n 1)


(* Question 3: Newton-Raphson method for computing the square root
*)

let square_root_tests = [
  (1.,1.);
  (4.,2.);
  (9.,3.);
  (16.,4.)
]

let square_root a =
  let rec findroot x acc =
    if abs_float (x -. (((a /. x) +. x) /. 2.)) < acc
    then (((a /. x) +. x) /. 2.)
    else findroot (((a /. x) +. x) /. 2.) acc 
  in
  if a > 0.
  then findroot 1. epsilon_float
  else domain ()


(* Question 4: Fibonacci*)

(* TODO: Write a good set of tests for fib_tl. *)
let fib_tl_tests = [
  (0, 1);
  (1, 1);
  (2, 2);
  (5, 8)
]

(* TODO: Implement a tail-recursive helper fib_aux. *)
let rec fib_aux n a b =
  if n = 0 || n = 1
  then b
  else fib_aux (n-1) b (a+b)

(* TODO: Implement fib_tl using fib_aux. *)
let fib_tl n =
  if n < 0 
  then domain()
  else
    fib_aux n 1 1
