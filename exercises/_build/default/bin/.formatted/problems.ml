(* Exercises inspired by Ninety-Nine Lisp Problems: https://ocaml.org/problems *)

(* Part 01: LISTS *)

(* Problem 01: Tail of a List (Beginner) *)

let rec last (lst : 'a list) : 'a option =
  match lst with [] -> None | [ x ] -> Some x | _ :: tl -> last tl

(* Problem 02: Last Two Elements of a List (Beginner) *)

let rec last_two (lst : 'a list) : ('a * 'a) option =
  match lst with
  | [] | [ _ ] -> None
  | [ x; y ] -> Some (x, y)
  | _ :: tl -> last_two tl

(* Prblem 03: N'th Element of a List (Beginner) *)

let rec at (lst : 'a list) (n : int) : 'a option =
  match lst with
  | hd :: _ when n = 0 -> Some hd
  | _ :: tl -> at tl (n - 1)
  | _ -> None

(* Problem 04: Length of a List (Beginner) *)

let length (lst : 'a list) : int =
  let rec aux (lst : 'a list) (n : int) : int =
    match lst with [] -> n | _ :: tl -> aux tl (n + 1)
  in
  aux lst 0

(* Problem 05: Reverse a List (Beginner) *)

let rev (lst : 'a list) : 'a list =
  let rec aux (lst : 'a list) (acc : 'a list) : 'a list =
    match lst with [] -> acc | hd :: tl -> aux tl (hd :: acc)
  in
  aux lst []

(* Problem 06: Palindrome (Beginner) *)

let is_palindrom (lst : 'a list) : bool = lst = rev lst

(* Problem 07: Flatten a List (Intermediate) *)

type 'a node = One of 'a | Many of 'a node list

let flatten (lst : 'a node list) : 'a list =
  let rec aux (lst : 'a node list) (acc : 'a list) : 'a list =
    match lst with
    | [] -> acc
    | One hd :: tl -> aux tl (hd :: acc)
    | Many hd :: t -> aux t (aux hd acc)
  in
  rev (aux lst [])

(* Problem 08: Eliminate Duplicates (Intermediate) *)

let compress2 (lst : 'a list) : 'a list =
  let rec already_in (lst : 'a list) (e : 'a) : bool =
    match lst with [] -> false | hd :: tl -> hd = e || already_in tl e
  in
  let rec aux (lst : 'a list) (acc : 'a list) : 'a list =
    match lst with
    | [] -> acc
    | h :: t when already_in acc h -> aux t acc
    | h :: t -> aux t (h :: acc)
  in
  rev (aux lst [])

(* TODO: this function eliminates all duplicates, not just consecutive ones *)

let rec compress (lst : 'a list) : 'a list =
  match lst with
  | hd :: (snd :: _ as tl) when hd = snd -> compress tl
  | hd :: tl -> hd :: compress tl
  | x -> x

(* Problem 09: Pack Consecutive Duplicates (Intermediate) *)

let pack (lst : int list) : int list list =
  let rec aux (lst : int list) (current : int list) (acc : int list list) :
      int list list =
    match lst with
    | [] -> [] (* ? *)
    | [ x ] -> (x :: current) :: acc
    | hd :: (snd :: _ as tl) when hd = snd -> aux tl (hd :: current) acc
    | hd :: tl -> aux tl [] ((hd :: current) :: acc)
  in
  rev (aux lst [] [])

(* Problem 10: Run-Length Encoding (Beginner) *)

let encode (lst : 'a list) : (int * 'a) list =
  let rec aux (lst : 'a list) (count : int) (acc : (int * 'a) list) :
      (int * 'a) list =
    match lst with
    | [] -> acc
    | [ x ] -> (count + 1, x) :: acc
    | hd :: (snd :: _ as tl) when hd = snd -> aux tl (count + 1) acc
    | hd :: tl -> aux tl 0 ((count + 1, hd) :: acc)
  in
  rev (aux lst 0 [])

(* Problem 11: Modified Run-Length Encoding (Beginner) *)

type 'a rle = One of 'a | Many of (int * 'a)

let rle_encode (lst : 'a list) : 'a rle list =
  let create_type (count : int) (element : 'a) : 'a rle =
    match count with 1 -> One element | _ -> Many (count, element)
  in
  let rec aux (lst : 'a list) (count : int) (acc : 'a rle list) : 'a rle list =
    match lst with
    | [] -> acc
    | [ x ] -> create_type (count + 1) x :: acc
    | hd :: (snd :: _ as tl) when hd = snd -> aux tl (count + 1) acc
    | hd :: tl -> aux tl 0 (create_type (count + 1) hd :: acc)
  in
  rev (aux lst 0 [])

(* Problem 12: Decode a Run-Length Encoded List (Intermediate) *)

let decode (lst : 'a rle list) : 'a list =
  let rec many (acc : 'a list) (n : int) (x : 'a) : 'a list =
    match n with 0 -> acc | _ -> many (x :: acc) (n - 1) x
  in
  let rec aux (lst : 'a rle list) (acc : 'a list) : 'a list =
    match lst with
    | [] -> acc
    | One x :: tl -> aux tl (x :: acc)
    | Many (n, x) :: tl -> aux tl (many acc n x)
  in
  rev (aux lst [])

(* Problem 13: Run-Length Encoding of a List (Direct Solution) (Intermediate) *)
(* Same solution as Problem 11 *)

(* Problem 14: Duplicate the Elements of a List (Beginner) *)

let duplicate (lst : 'a list) : 'list =
  let rec aux (lst : 'a list) (acc : 'a list) =
    match lst with [] -> acc | hd :: tl -> aux tl (hd :: hd :: acc)
  in
  rev (aux lst [])

(* Problem 15: Replicate the Elements of a List a Given Number of Times (Intermediate) *)

let replicate (lst : 'list) (n : int) : 'a list =
  let rec put_first (acc : 'a list) (element : 'a) (n : int) : 'a list =
    match n with 0 -> acc | _ -> put_first (element :: acc) element (n - 1)
  in
  let rec aux (lst : 'list) (acc : 'a list) =
    match lst with [] -> acc | hd :: tl -> aux tl (put_first acc hd n)
  in
  rev (aux lst [])

(* Problem 16: Drop every N'th Element From a List (Intermediate) *)

let drop (lst : 'a list) (n : int) : 'a list =
  let rec aux (lst : 'a list) (acc : 'a list) (i : int) : 'a list =
    match lst with
    | [] -> acc
    | _ :: tl when i = n -> aux tl acc 1
    | hd :: tl -> aux tl (hd :: acc) (i + 1)
  in
  rev (aux lst [] 1)

(* Problem 17: Split a List Into Two Parts; The Length of the First Part Is Given (Beginner) *)

let split (lst : 'a list) (n : int) : 'a list * 'a list =
  let rec aux (lst : 'a list) (acc : 'a list) (n : int) : 'a list * 'a list =
    match lst with
    | [] -> (rev acc, [])
    | hd :: tl when n > 0 -> aux tl (hd :: acc) (n - 1)
    | _ -> (rev acc, lst)
  in
  aux lst [] n

(* Problem 18: Extract a Slice From a List (Intermediate) *)

let slice (lst : 'a list) (i : int) (j : int) : 'a list =
  let rec aux (lst : 'a list) (acc : 'a list) (min : int) (max : int) : 'a list
      =
    match lst with
    | [] -> acc
    | hd :: tl when min <= 0 && max >= 0 ->
        aux tl (hd :: acc) (min - 1) (max - 1)
    | _ :: tl -> aux tl acc (min - 1) (max - 1)
  in
  rev (aux lst [] (min i j) (max i j))

(* Problem 19: Rotate a List N Places to the Left (Intermediate) *)

let rotate (lst : 'a list) (n : int) : 'a list =
  let rec aux (lst : 'a list) (acc : 'a list) (n : int) : 'a list =
    match lst with
    | [] -> rev acc
    | _ :: _ when n = 0 -> lst @ rev acc
    | hd :: tl -> aux tl (hd :: acc) (n - 1)
  in
  aux lst [] n

(* Problem 20: Remove the K'th Element form a List (Beginner) *)

let rec remove_at k = function
  | [] -> []
  | _ :: tl when k = 0 -> tl
  | hd :: tl -> hd :: remove_at (k - 1) tl

(* Problem 21: Insert an Element at a Given Position Into a List (Beginner) *)

let rec insert_at element n = function
  | [] -> [ element ]
  | lst when n = 0 -> element :: lst
  | hd :: tl -> hd :: insert_at element (n - 1) tl

(* Problem 22: Create a List Containing All Integers Within a Given Range (Beginner) *)

let range a b =
  let rec aux acc i j = if i > j then acc else aux (i :: acc) (i + 1) j in
  if a <= b then rev (aux [] a b) else aux [] b a

(* Problem 23: Extract a Given Number of Randomly Selected Elements From a List (Intermediate) *)

let rand_select lst n =
  let rand_rotate = function
    | [] -> []
    | l -> rotate l (Random.int (length l))
  in
  let rec aux n acc = function
    | [] -> acc
    | _ :: _ when n <= 0 -> acc
    | hd :: tl -> aux (n - 1) (hd :: acc) (rand_rotate tl)
  in
  aux n [] (rand_rotate lst)

(* Problem 24: Lotto: Draw N Different Random Numbers From the Set 1..M (Beginner) *)

let lotto_select n m = rand_select (range 1 m) n

(* Problem 25: Generate a Random Permutation of the Elements of a List (Beginner) *)

let permutation = function [] -> [] | lst -> rand_select lst (length lst)

(* Problem 26: Generate the Combinations of K Distinct Objects Chosen From the N Elements of a List (Intermediate) *)

(* FIXME: For now only produces combinations of 2 elements *)
let extract k lst =
  let rec aux2 a acc = function
    | [] -> acc (* should not be reached*)
    | [ x ] -> (a :: [ x ]) :: acc
    | hd :: tl -> aux2 a ((a :: [ hd ]) :: acc) tl
  in
  let rec aux n acc = function
    | [] -> acc
    | [ _ ] -> acc
    | hd :: tl -> aux (n - 1) (aux2 hd [] tl @ acc) tl
  in
  rev (aux k [] lst)

(* FIXME: Attempt at solution with more than two values per Combination *)
let extract2 k lst =
  let rec aux n current acc = function
    | [] -> acc
    | hd :: tl when n >= 1 -> aux k [] (aux (n - 1) (hd :: current) acc tl) tl
    | _ :: _ -> rev (rev current :: acc)
  in
  aux k [] [] lst

(* Solution provided by website *)
let rec extract3 k (lst : int list) =
  if k <= 0 then [ [] ]
  else
    match lst with
    | [] -> []
    | hd :: tl ->
        let with_hd = List.map (fun l -> hd :: l) (extract3 (k - 1) tl) in
        let without_hd = extract3 k tl in
        with_hd @ without_hd

(* Not an exercise; delete later *)
let map f lst =
  let rec aux f acc = function
    | [] -> acc
    | hd :: tl -> aux f (f hd :: acc) tl
  in
  rev (aux f [] lst)

(* Problem 27: Group the Elements of a Set Into Disjoint Subsets (Intermediate) TODO *)
(* let group lst1 lst2 = ... *)

(* Problem 28: Group the Elements of a Set Into Disjoint Subsets (Intermediate) TODO *)
(* 1. let length_sort lst *)
(* 2. let frequency_sort lst *)

(* Part 02: ARITHMETIC *)

(* Problem 29: Determine Whether a Given Number Is Prime (Intermediate) *)

let is_prime n =
  if n <= 1 then false
  else
    let rec is_not_divisor k =
      k * k > n || (n mod k <> 0 && is_not_divisor (k + 1))
    in
    is_not_divisor 2

(* Problem 30: Determine the Greatest COmmon Divisor of Two Positive Integer Numbers (Intermediate) *)

let rec gcd a b = if a = 0 then b else gcd (b mod a) a

(* Problem 31: Determine Wheter Two Positive Integer Numbers Are Coprime (Beginner) *)

let coprime a b = gcd a b = 1

(* Problem 32: Calculate Euler's Totient Function Phi(m) (Intermediate) *)

let phi m =
  let rec count_coprime r acc =
    if r < m then count_coprime (r + 1) (if coprime r m then acc + 1 else acc)
    else acc
  in
  count_coprime 2 1

(*  Problem 32: Determine the Prime Factors of a Given Positive Integer (Intermediate) *)

let factors n =
  let rec aux n d acc =
    if n = 1 then rev acc
    else if n mod d = 0 then aux (n / d) d (d :: acc)
    else aux n (d + 1) acc
  in
  aux n 2 []

(* Problem 33: Determine the Prime Factors of a Given Positive Integer (2) (Intermediate) *)
(* Construct a list containing the prime factors and their multiplicity *)
(* this results in a (m * p) list where m is the factor and p is the prime number *)

let factors2 n = encode (factors n)

(* Problem 34: Calculate Euler's Totient Function phi(m) (Improved) (Intermediate) *)

(* Simple power function *)
let power a b =
  let rec aux a b res = if b < 1 then res else aux a (b - 1) (res * a) in
  aux a b 1

let phi_improved m =
  let rec aux acc = function
    | [] -> acc
    | (m, p) :: tl -> aux (acc * (p - 1) * power p (m - 1)) tl
  in
  aux 1 (factors2 m)

(* Problem 35: Compare thw Two Methods of Calculating Euler' Totient Function (Beginner) *)
(* TODO: Maybe do it later (not very interesting) *)

(* Problem 36: A List of Prime Numbers (Beginner) *)

let all_primes a b =
  let rec aux n acc =
    if n <= b then aux (n + 1) (if is_prime n then n :: acc else acc) else acc
  in
  rev (aux a [])

(* Problem 37: Goldbach's Conjecture (Intermediate) *)

let goldbach m =
  if m > 2 && m mod 2 = 0 then
    let rec aux d =
      if is_prime d && is_prime (m - d) then (d, m - d) else aux (d + 1)
    in
    aux 2
  else (0, 0)

(* Problem 38: A List of Goldbach Compositions (Intermediate) *)

let goldbach_list a b =
  let rec aux n acc =
    if n <= b then aux (n + 2) ((n, goldbach n) :: acc) else rev acc
  in
  aux (a + (a mod 2)) []

(* LOGIC AND CODES *)

(* Problem 39: Truth Tables for Logical Expressions (2 Variables) (Intermediate) *)

type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr

let table2 a b expr =
  let rec eval2 a val_a b val_b = function
    | Var x ->
        if x = a then val_a
        else if x = b then val_b
        else failwith "Invalid argument!"
    | Not e -> not (eval2 a val_a b val_b e)
    | And (e1, e2) -> eval2 a val_a b val_b e1 && eval2 a val_a b val_b e2
    | Or (e1, e2) -> eval2 a val_a b val_b e1 || eval2 a val_a b val_b e2
  in
  [
    (true, true, eval2 a true b true expr);
    (true, false, eval2 a true b false expr);
    (false, true, eval2 a false b true expr);
    (false, false, eval2 a false b false expr);
  ]

(* Problem 40: Truth Tables for Logical Expressions (Intermediate) *)

let table variables expr =
  let rec get a = function
    | [] -> failwith "Invalid argument!"
    | (fst, snd) :: _ when a = fst -> snd
    | _ :: tl -> get a tl
  in
  let rec eval val_vars = function
    | Var x -> get x val_vars
    | Not e -> not (eval val_vars e)
    | And (e1, e2) -> eval val_vars e1 && eval val_vars e2
    | Or (e1, e2) -> eval val_vars e1 || eval val_vars e2
  in
  let rec create_table acc vars expr =
    match vars with
    | [] -> [ (rev acc, eval acc expr) ]
    | hd :: tl ->
        create_table ((hd, true) :: acc) tl expr
        @ create_table ((hd, false) :: acc) tl expr
  in
  create_table [] variables expr

(* Problem 41: Gray Code (Intermediate) *)

let gray n =
  (* pad each String in this list with the string str *)
  let rec pad_with str = function
    | [] -> []
    | hd :: tl -> (str ^ hd) :: pad_with str tl
  in
  let rec aux i acc =
    if i < n then
      let fst = pad_with "0" acc in
      let snd = pad_with "1" acc in
      aux (i + 1) (fst @ rev snd)
    else acc
  in
  aux 1 [ "0"; "1" ]
