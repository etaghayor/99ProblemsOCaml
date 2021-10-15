
(* 31 - 45 *)



let is_prime n = let rec aux n d = match n with
    | 0 | 1 -> false
    | n -> if d <= 1 then true else
        n mod d <> 0 && aux n (d-1) in aux n (int_of_float (sqrt (float_of_int n)))

let rec gcd a b = 
  if a mod b = 0 then b else gcd b (a mod b)

let coprime a b = gcd a b = 1

let phi n = let rec aux res n = function
    | 1 -> res + 1
    | d -> if (coprime n d) then aux (res+1) n (d-1) else aux res n (d-1) in aux 0 n (n)

let factors n = let rec aux res d n =
                  if d > n then res else 
                  if is_prime d && n mod d = 0 then aux (d::res) 2 (n/d)
                  else aux res (d+1) n
  in List.rev(aux [] 2 n)

let encode l = 
  let rec aux count res = function
    | [] -> []
    | [x] -> (x,(count+1))::res
    | x::(y::_ as l) ->  if x=y then aux (count+1) res l else aux 0 ((x,count+1)::res) l
  in List.rev (aux 0 [] l)

let factors_and_pow n = encode (factors n)