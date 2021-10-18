
(* Arithmetics *)
(* Problems 31 - 45 *)

(* 31 *)
let is_prime n = let rec aux n d = match n with
    | 0 | 1 -> false
    | n -> if d <= 1 then true else
        n mod d <> 0 && aux n (d-1) in aux n (int_of_float (sqrt (float_of_int n)))

(* 32 *)
let rec gcd a b = 
  if a mod b = 0 then b else gcd b (a mod b)

(* 33 *)
let coprime a b = gcd a b = 1

(* 34 *)
let phi n = let rec aux res n = function
    | 1 -> res + 1
    | d -> if (coprime n d) then aux (res+1) n (d-1) else aux res n (d-1) in aux 0 n (n)

(* 35 *)
let factors n = let rec aux res d n =
                  if d > n then res else 
                  if is_prime d && n mod d = 0 then aux (d::res) 2 (n/d)
                  else aux res (d+1) n
  in List.rev(aux [] 2 n)

(* 36 *)
let encode l = 
  let rec aux count res = function
    | [] -> []
    | [x] -> (x,(count+1))::res
    | x::(y::_ as l) ->  if x=y then aux (count+1) res l else aux 0 ((x,count+1)::res) l
  in List.rev (aux 0 [] l)

let factors_and_pow n = encode (factors n)

(* 37 *)
let pow a b = int_of_float (Float.pow (float_of_int a) (float_of_int b))

let phi_improved n = let rec aux res = function 
    | [] -> res
    | (x,p)::xs -> aux ((x-1)*(pow x (p-1)) * res) xs
  in aux 1 (factors_and_pow n)

(* 38 *)
 let timeit f a =
    let t0 = Unix.gettimeofday() in
      ignore (f a);
    let t1 = Unix.gettimeofday() in
      t1 -. t0

(* 39 *)
let all_primes n m =
  let rec aux res n m = 
    if n <= m then 
      if is_prime n then aux (n::res) (n+1) m else aux res (n+1) m 
    else res in aux [] n m

(* 40 *)
let goldbach n = 
  let rec aux a b =
    if (is_prime a && is_prime b ) then (a,b) else
      aux (a+1) (b-1) in aux 0 n

(* 41 *)
let goldbach_list n m = 
  let rec aux res n m =
    if (n <= m) then aux ((n,(goldbach n))::res) (n+2) m else res
  in List.rev(aux [] ((n + 1) / 2 * 2)((m + 1) / 2 * 2))

(* 42 *)
let goldbach_limit n m limit= 
  List.filter (fun (n,(g1,g2)) -> g1>limit) (goldbach_list n m)

