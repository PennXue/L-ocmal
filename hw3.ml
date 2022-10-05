(* TODO: Write some tests for tabulate. *)
(* Note: we've added a type annotation here so that the compiler can help
   you write tests of the correct form. *)
let tabulate_tests: (((int -> int) * int) * int list) list = [
  (* Remember: your test cases should have this form:
     ((f, n), output)
     The following test case asserts that:
       tabulate (fun x -> x) (-1)
     should have the output []
  *)
  (((fun x -> x+x),  3), [0; 2; 4; 6]);
  (((fun x -> x),  1), [0; 1]);
  (((fun x -> x),  0), [0]);
  (((fun x -> x), -1), [])
]

(* TODO: Implement dist_table: (int * int) -> int -> float list *)
let dist_table ((marblesTotal, marblesDrawn): (int * int)) (x: int) : float list =
  let y = x in
  tabulate (fun x -> dist_black x y (marblesTotal, marblesDrawn)) marblesTotal;;
  

(* TODO: Write some test cases for is_empty. *)
let is_empty_tests: (float list list * bool) list = [
  ([], true);
  ([[]], true);
  ([[1.]], false)
]

(* TODO: Implement is_empty: 'a list list -> bool *)
let is_empty (matrix: 'a list list) : bool =
  List.for_all (fun x -> x = []) matrix

(* TODO: Implement dist_matrix: int * int -> int list -> float list list *)
let dist_matrix ((total, drawn): int * int) (resultList: int list) : float list list =
  List.map (fun x -> dist_table (total, drawn) x) resultList

(* TODO: Implement combined_dist_table: float list list -> float list *)
let combined_dist_table (matrix: float list list) = 
  let rec headExp matrix acc = 
    if (is_empty matrix)
    then acc
    else 
      headExp (List.map (fun a -> (List.tl a)) matrix) (acc @ [(List.fold_left (fun a b -> a *. (List.hd b)) 1. matrix)])
  in headExp matrix []
      

(* Once you have implemented all the above functions, you can
   use this function to compute the maximum likelihood.
   You can try it on the given example by running:
     max_likelihood (6, 3) [2; 0; 1]
*)
let max_likelihood (total, drawn) resultList =
  max_in_list
    (combined_dist_table
       (dist_matrix (total, drawn) resultList))


(* TODO: Implement all: (ingredients list -> bool) -> cake -> bool *)
let rec all (p: (ingredients list -> bool)) (c: cake) : bool = 
  match c with 
  | Slice s -> p s
  | Cake (s1, s2) -> all p s1 && all p s2
  
(* TODO: Write some test cases for is_chocolate_cake. *)
let is_chocolate_cake_tests = [
  (Slice[], false);
  (Slice[Flour; Almonds], false);
  (Slice[Chocolate; Flour], true);
  (Cake(Slice[Chocolate ; Flour],Slice[Flour; Almonds]), false); 
]

(* TODO: Implement is_chocolate_cake: cake -> bool *)
let is_chocolate_cake (c: cake) : bool = 
  all (fun x -> List.exists(fun y -> y = Chocolate) x) c
    

(* TODO: Implement map: (ingredients list -> ingredients list) -> cake -> cake *)
let rec map (p: (ingredients list -> ingredients list)) (c: cake) = 
  match c with
  | Slice s -> Slice(p s)
  | Cake (s1, s2) -> Cake((map p s1),(map p s2))
  

(* TODO: Write some test cases for add_ingredient. *)
let add_ingredient_tests = [
  ((Chocolate, Slice([])), Slice([Chocolate])); 
  ((Chocolate, Slice([Flour])), Slice([Flour;Chocolate]));
  ((Chocolate, Cake(Slice[Flour], Slice[Chocolate])), Cake(Slice[Flour;Chocolate], Slice[Chocolate]))
]

(* TODO: Implement add_ingredient: ingredients -> cake -> cake *)
let add_ingredient (x: ingredients) (c: cake) : cake = 
  map (fun l -> insert x l) c   

(* TODO: Implement fold_cake: (ingredients list -> 'a -> 'a) -> 'a -> cake -> 'a  *)
let rec fold_cake (f: (ingredients list -> 'a -> 'a)) (base: 'a) (c: cake) : 'a = 
  match c with
  | Slice s -> f s base
  | Cake(s1, s2) -> fold_cake f (fold_cake f base s1) s2


(* TODO: Implement get_all_ingredients: cake -> ingredients list *)
let get_all_ingredients (c: cake) : ingredients list = 
  fold_cake (fun l1 -> union l1) [] c