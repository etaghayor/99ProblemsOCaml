open List;;

(* 1-15 *)

let rec last = function
  | [] -> None
  | [x] -> Some x
  | x::xs -> last xs;;

let rec last_two = function
  | [] | [_] -> None
  | x::y::[] -> Some (x,y)
  | x::xs -> last_two xs;;

let rec at n l = match n,l with
  | _ , [] -> None
  | 1 , x::xs -> Some x
  | n , x::xs -> at (n-1) xs

let length l =
  let rec aux res = function
    | [] -> res
    | _::xs -> aux (res+1) xs
  in aux 0 l

let rev l = 
  let rec aux res = function
    | [] -> res
    | x::xs -> aux (x::res) xs
  in aux [] l

let is_palindrome l =  l = rev l 

type 'a node =
  | One of 'a 
  | Many of 'a node list;;

let rec flatten l =    (* bugged in this one *)
  let rec aux res = function
    | [] -> res
    | One x :: xs -> aux (x::res) xs
    | Many x :: xs -> aux (aux res x) xs in
  rev (aux [] l)

let rec compress = function (*coul've been shorter*)
  | x::(y::_ as l) ->  if x=y then compress l else x::(compress l)
  | smaller -> smaller

let pack l = 
  let rec aux sublist res = function
    | [] -> []
    | [x] -> (x::sublist)::res
    | x::(y::_ as l) ->  if x=y then aux (x::sublist) res l else aux [] ((x::sublist)::res) l
  in rev (aux [] [] l)

let encode l = 
  let rec aux count res = function
    | [] -> []
    | [x] -> ((count+1),x)::res
    | x::(y::_ as l) ->  if x=y then aux (count+1) res l else aux 0 ((count+1,x)::res) l
  in rev (aux 0 [] l)

type 'a rle =
  | One of 'a
  | Many of int * 'a;;

let modified_encode l =
  let encoded = encode l in
  let rec aux = function
    | [] -> []
    | (1,y)::xs -> (One y) :: aux xs
    | (x,y)::xs -> (Many (x,y))::aux xs
  in aux encoded

let decode l = 
  let rec aux res = function
    | [] -> res
    | One x :: xs -> aux (x::res) xs
    | Many (0,y)::xs -> aux res xs
    | Many (x,y)::xs -> aux (y::res) (Many(x-1,y)::xs)
  in rev (aux [] l)


let rec duplicate = function
  | [] -> []
  | [x] -> x::x::[]
  | x::xs -> x::x::(duplicate xs)

let rec replicate l n =  (*not good *)
  let rec repeat x = function
    | 1 -> [x]
    | k -> x::(repeat x (k-1)) in
  let rec aux res n l = match l with
    | [] -> res
    | x::xs -> aux ((repeat x n) @ res) n xs

  in rev (aux [] n l)