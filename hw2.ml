(* Reminder: If a test case requires multiple arguments, use a tuple:
let myfn_with_2_args_tests = [
  ((arg1, arg1), (expected_output))
]
*)

(* Q1 *)
(* TODO: Write a good set of tests for compress *)
let compress_tests = [
  ([A;A;G;G;T;T;C;C], [(2,A);(2,G);(2,T);(2,C)]);
  ([A;A;A;G;G;T;C], [(3,A);(2,G);(1,T);(1,C)]);
  ([A], [(1,A)]);
  ([], [])
]

(* TODO: Implement compress. *)
let compress (l : nucleobase list) : (int * nucleobase) list =
  let rec head l acc result = 
    match l with 
    | [] -> [];
    | h1 :: rest1 -> 
        (match rest1 with
         | [] -> result @ [(acc+1, h1)];
         | h2 :: _ -> 
             if h1 = h2
             then head rest1 (acc+1) result
             else 
               head rest1 0 (result @ [((acc+1), h1)])
        ) 
  in head l 0 [];;
  

(* TODO: Write a good set of tests for decompress *) 
let decompress_tests = [
  ([(2,A);(2,G);(2,T);(2,C)],[A;A;G;G;T;T;C;C]);
  ([(3,A);(2,G);(1,T);(1,C)],[A;A;A;G;G;T;C]);
  ([(1,A)],[A]);
  ([], [])
]

(* TODO: Implement decompress. *)
let decompress (l : (int * nucleobase) list) : nucleobase list =
  let rec head l acc result = 
    match l with 
    | [] -> result
    | (a, b) :: r1 ->
        if acc = a
        then head r1 0 result
        else head l (acc+1) (result @ [b])
  in head l 0 [];;
(* Q2 *)
(* TODO: Write a good set of tests for eval *)
let eval_tests = [
  (MULT (PLUS (FLOAT 2.2, FLOAT 3.3), FLOAT 5.0), 27.5); 
  (PLUS (FLOAT 1.0, FLOAT 0.0), 1.0);
  (EXP (FLOAT 0.0), 1.);
  (SIN (FLOAT 0.), 0.);
  (COS (FLOAT 0.), 1.);
  ((FLOAT 1.) , 1.)
  
]

(* TODO: Implement eval. *)
let rec eval e =
  match e with
  | FLOAT a -> a
  | PLUS(a, b) -> (eval a) +. (eval b)
  | MINUS(a, b) -> (eval a) -. (eval b)
  | MULT(a, b) -> (eval a) *. (eval b)
  | DIV(a, b) -> (eval a) /. (eval b)
  | SIN(a) -> sin(eval a) 
  | COS(a) -> cos(eval a)
  | EXP(a) -> exp(eval a)
                            

(* TODO: Write a good set of tests for to_instr *)
let to_instr_tests = [
  (MULT (PLUS (FLOAT 2.2, FLOAT 3.3), FLOAT 5.0), [Float 2.2; Float 3.3; Plus; Float 5.; Mult]); 
  (PLUS (FLOAT 1.0, FLOAT 0.0), [Float 1.0; Float 0.0; Plus]);
  (EXP (FLOAT 0.0), [Float 0.0; Exp]);
  (SIN (FLOAT 0.), [Float 0.0; Sin]);
  (COS (FLOAT 0.), [Float 0.0; Cos]);
  ((FLOAT 1.) , [Float 1.0])
]

(* TODO: Implement to_instr. *)
let rec to_instr e = 
  match e with
  | FLOAT a -> [Float a]
  | PLUS(a, b) -> ((to_instr a) @ (to_instr b) @ [Plus])
  | MINUS(a, b) -> ((to_instr a) @ (to_instr b) @ [Minus])
  | MULT(a, b) -> ((to_instr a) @ (to_instr b) @ [Mult])
  | DIV(a, b) -> ((to_instr a) @ (to_instr b) @ [Div])
  | SIN(a) -> ((to_instr a) @ [Sin]) 
  | COS(a) -> ((to_instr a) @ [Cos])
  | EXP(a) -> ((to_instr a) @ [Exp])

                  


(* TODO: Write a good set of tests for instr *)
let instr_tests = [
  (((Float 4.2),[2.2; 3.3; 5.5]), Some [4.2; 2.2; 3.3; 5.5]);
  ((Mult, [5.0; 5.5]),(Some [27.5]));
  ((Plus,[2.2; 3.3; 5.0]), (Some [5.5; 5.])); 
  ((Sin, [0.0]), Some [0.0])
]


(* TODO: Implement to_instr. *)               
let instr i s = 
  match i with
  | (Float a)  -> Some(a :: s)
  | Plus -> 
      (match s with 
       |[] -> None
       |[a] -> None
       |x::y::rest -> Some((y +. x) :: rest)
      )
  | Minus -> 
      (match s with
       |[] -> None
       |[a] -> None
       |x::y::rest -> Some((y -. x) :: rest)
      )
  | Mult ->
      (match s with
       |[] -> None
       |[a] -> None
       |x::y::rest -> Some((y *. x) :: rest)
      )
  | Div ->
      (match s with
       |[] -> None
       |[a] -> None
       |x::y::rest -> Some((y /. x) :: rest)
      )
  | Sin ->
      (match s with
       |[] -> None 
       |x::rest -> Some((sin x) :: rest)
      )
  | Cos ->
      (match s with
       |[] -> None
       |x::rest -> Some((cos x) :: rest)
      )
  | Exp ->
      (match s with
       |[] -> None
       |x::rest -> Some((exp x) :: rest)
      );;
  
(* TODO: Write a good set of tests for prog *)
let prog_tests = [
  ([Float 2.2; Float 3.3; Plus; Float 5.; Mult], Some 27.5);
  ([Float 0.0; Exp], Some 1.0);
  ([Float 0.0; Sin], Some 0.0);
  ([Float 0.0; Cos], Some 1.0);
  ([Float 1.0], Some 1.0)
  
] 
(* TODO: Implement prog. *)
let prog instrs = 
  let rec cal l instrs = 
    match instrs with
    | [] -> ( 
        match l with 
        | [] -> None
        | head :: rest -> Some head
      )
    | h :: rest -> 
        match h with
        | Float a -> cal (a :: l) rest
        | Plus -> (
            match l with
            | x :: y :: _ -> cal ([x +. y]) rest
            | _ -> None
          )
        | Minus -> (
            match l with 
            | x :: y :: _ -> cal ([y -. x]) rest
            | _ -> None
          )
        | Mult -> (
            match l with 
            | x :: y :: _ -> cal ([y *. x]) rest
            | _ -> None
          )
        | Div -> (
            match l with 
            | x :: y :: _ -> cal ([y /. x]) rest
            | _ -> None
          )
        | Sin -> (
            match l with 
            | [] -> None
            | x :: _ -> cal ( (sin x) :: l) rest 
          )
        | Cos -> (
            match l with 
            | [] -> None
            | x :: _ -> cal ((cos x) :: l) rest 
          )
        | Exp -> (
            match l with 
            | [] -> None
            | x :: _ -> cal ( (exp x) :: l) rest 
          )
  in cal [] instrs;;
            
            
            
    
    








