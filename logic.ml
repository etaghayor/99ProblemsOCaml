(* Logic and Codes *)
(* Problems 46 - 50 *)

type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr;;


(* 46,47 *)
let table2 a b expr = 
  let rec aux a b = function
    | Var v -> if (v = "a") then a else b
    | Not exp -> not (aux a b exp)
    | And (exp1, exp2) -> (aux a b exp1 ) && (aux a b exp2)
    | Or (exp1, exp2) -> (aux a b exp1 ) || (aux a b exp2)
  in ((true,true,aux true true expr))::((true,false,aux true false expr))::
     ((false,true,aux false true expr))::((false,false,aux false false expr))::[]

(* 48 *)
let add_to_all x l = List.map (fun e -> x::e) l

let rec interpretations_props props = 
  let rec aux res = function
    | [] -> res
    | x::xs -> aux (add_to_all (x,true) res @ (add_to_all (x, false) res)) xs
  in (aux [[]] (List.rev props))

let rec eval vals = function
  | Var v -> List.assoc v vals
  | Not exp -> not (eval vals exp)
  | And (exp1, exp2) -> (eval vals exp1 ) && (eval vals exp2)
  | Or (exp1, exp2) -> (eval vals  exp1 ) || (eval vals exp2)

let table props expr = 
  List.map (fun x -> (x, eval x expr)) (interpretations_props props)

(* 49 *)
let add_to_all_string x l = List.map (fun e -> x^e) l

let gray n = let rec aux res = function 
    | 1 -> res
    | n -> aux ((add_to_all_string "0" res) @ (List.rev (add_to_all_string "1" res))) (n-1)
  in aux ["0";"1"] n

(* 50 *)
type int_tree =
  | Nil
  | Node of (string * int) * int_tree * int_tree

let rec heap = function
  | [] -> []
  | (x,fx)::xs -> Node((x,fx), Nil,Nil)::(heap xs)

let rec add_node_list (Node ((_,t_val),_,_) as t) l = 
  let rec aux res t t_val = function
    | [] -> List.rev (t::res)
    | (Node((_,x),_,_) as n)::xs -> if (x>t_val) then (List.rev (n::t::res))@xs else aux (n::res) t t_val xs
    | _ -> res
  in aux [] t t_val l

let rec heap_huffman = function
  | (Node((x,fx),_, _) as l)::(Node((y,fy),_, _)as r)::xs ->heap_huffman (add_node_list (Node(("",fx+fy), l, r)) xs)
  | t -> t

let rec code_huffman code = function
  | Node ((x,fx) , Nil, Nil) -> [(x,code)]
  | Node (("",fx), y,z) -> (code_huffman (code^"1") z) @ (code_huffman (code^"0") y)
  | t -> []

let huffman l = code_huffman "" (List.hd(heap_huffman (heap (List.sort ( fun (_,x)-> fun (_,y) ->x-y )l))))

