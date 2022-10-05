(* ------------------------------------------------------------------------*)
(* Q 1 : Money in the bank (25 points)                                     *)
(* ------------------------------------------------------------------------*)

let new_account (p: passwd) : bank_account = 
  let bankamount = ref 0 in
  let attempt = ref 0 in
  let passwd = ref p in
  let update_passwd passwd1 passwd2 = 
    if passwd1 = !passwd 
    then (attempt := 0;
          passwd := passwd2)
    else (attempt := !attempt + 1;
          raise wrong_pass) 
  in
  let deposit passwd1 amount = 
    if !attempt >= 3
    then raise too_many_attempts
    else (
      if passwd1 = !passwd
      then
        (bankamount := !bankamount + amount;
         attempt := 0)
      else(attempt := !attempt + 1;
           raise wrong_pass))
  in
  let retrieve passwd1 amount = 
    if !attempt >= 3
    then raise too_many_attempts
    else (if passwd1 = !passwd
          then (if amount <= !bankamount
                then (bankamount := !bankamount - amount;
                      attempt := 0)
                else raise no_money)
          else 
            (attempt := !attempt + 1;
             raise wrong_pass)
         )
  in 
  let print_balance passwd1 = 
    if !attempt >= 3
    then raise too_many_attempts
    else(if passwd1 = !passwd
         then (attempt := 0;
               !bankamount)
         else
           (attempt := !attempt + 1;
            raise wrong_pass))
  in
  {
    update_passwd = update_passwd;
    retrieve = retrieve;
    deposit = deposit;
    print_balance = print_balance
  }
;;


(* ------------------------------------------------------------------------*)
(* Q 2 : Memoization (75 points)                                           *)
(* ------------------------------------------------------------------------*)

(* Q 2.1 : Counting how many function calls are made *)

let rec fib_I (n: int) : fib_result =
  let count = ref 1 in
  let rec f n =
    if n < 2 
    then n
    else (
      count := !count + 2;
      f (n-2) + f (n-1);
    ) 
  in
  { num_rec = !count;
    result = f n; }
;;


(* Q 2.2 : Memoization with a global store *)

let fib_memo (n: int) : int = 
  let rec fib n =
    match (Hashtbl.find_opt store n) with
    | Some v -> v;
    | None -> (if n < 2
               then (Hashtbl.add store n n; 
                     n)
               else (let add_value = fib (n-2) + fib(n-1) in
                     Hashtbl.add store n add_value;
                     add_value))
  in
  fib n
;;


(* Q 2.3 : General memoization function *)

let memo (f: (('a -> 'b) -> 'a -> 'b)) (stats: stats) : ('a -> 'b) =
  let store = Hashtbl.create 1000 in
  let lkp = stats.lkp in
  let entries = stats.entries in
  let rec mem a =
    match Hashtbl.find_opt store a with 
    | Some v -> (stats.lkp := !lkp + 1;
                 v) 
    | None -> 
        (let result = f mem a in
         stats.entries := !entries + 1;
         Hashtbl.add store a result;
         result)
  in mem
        
;;


(* Q 2.4 : Using memo to efficiently compute the Fibonacci number *)
(* We also accept let fibM = raise NotImplemented ;; *)
let fibM =
  let entries = ref 0 in
  let lkp = ref 0 in
  let stats = 
    { entries = entries;
      lkp = lkp;}
  in
  let result = 
    (memo (fun fib n -> 
         if n < 2
         then n
         else fib(n-2) + fib(n-1)) stats)
  in (fun n -> result n, stats)
;;
