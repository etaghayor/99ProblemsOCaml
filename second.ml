
(* 16-30 *)

let drop l k =
  let rec aux res l i = match l,i with
    | [], _ -> res
    | x::xs , 1 -> aux res xs (k)
    | x::xs , j -> aux (x::res) xs (j-1)
  in List.rev (aux [] l k)

let split l k =
  let rec aux res1 res2 l i = match l,i with
    | [], _ -> (res1, res2)
    | x::xs , 0 -> aux res1 (x::res2) xs 0
    | x::xs , j -> aux (x::res1) res2 xs (j-1)
  in let (res1,res2) = (aux [] [] l k) in
  (List.rev res1, List.rev res2)

let slice l k kk = 
  let rec aux res l i j = match l,i,j with
    | [] , _, _ -> res
    | x::xs , _ , -1 -> res
    | x::xs , 0 , j -> aux (x::res) xs 0 (j-1)
    | x::xs , i,j -> aux res xs (i-1) (j-1)
  in List.rev (aux [] l k kk )

let rotate l n = 
  let len = List.length l - 1 in
  if n > 0 then slice l n len @ (slice l 0 (n-1)) else (slice l (len+n+1) len) @ (slice l 0 (len+n))

let remove_at k l =
  let rec aux res k l = match l,k with
    | [], _ -> res
    | x::xs , 0 -> aux res (-1) xs
    | x::xs , k -> aux (x::res) (k-1) xs
  in List.rev (aux [] k l) 

let insert_at e k l =
  let rec aux res e k l = match l,k with
    | [],_ -> res
    | x::xs, 1 -> aux (e::x::res) e 0 xs
    | x::xs, k -> aux (x::res) e (k-1) xs
  in List.rev (aux [] e k l) 

let range x y =
  let rec aux res x y =
    if (x > y) then aux (x::res) (x-1) y else
    if (x < y) then aux (x::res ) (x+1) y else
      x::res 
  in List.rev (aux [] x y )

let rand_select l n =
  let rec choose_and_remove l res k=
    match l,k with
    | [], x -> failwith "out of bounds"
    | x::xs , 0 -> res @ xs, x
    | x::xs, m -> choose_and_remove xs ( List.rev (x::(List.rev res))) (m-1)
  and aux l res k =
    match l,k with
    | [] , x -> []
    | x::xs , 0 -> x::res
    | _ , m -> 
      let t,x = choose_and_remove l [] (Random.int (List.length l)) 
      in (aux t (x::res) (m-1))
  in aux l [] (n-1)

let rec lotto_select k m = rand_select (range 1 m) k

let permutation l = rand_select l (List.length l)

let rec aux_not_exists e = function
  | [] -> true
  | x::xs -> if x=e then false else aux_not_exists e xs

let rec extract k l =
  if k <= 0 then [[]]
  else match l with
    | [] -> []
    | x::xs -> (List.map (fun e -> x::e) (extract (k-1) xs)) @ (extract (k) xs)

(* let rec group l gs =
(* let rec aux   *)
  | [] -> l
  | x::xs ->  *)