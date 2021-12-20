type log_exp =
Const of bool
| Var of string
| Neg of log_exp
| Disj of log_exp * log_exp
| Conj of log_exp * log_exp
| Cond of log_exp * log_exp
| BiCond of log_exp * log_exp;;

type oper = Not;;
type biOper = Or | And | If | Iff;;
type prop =
C of bool
| V of string
| Op of oper * prop
| BiOp of biOper * prop * prop;;

let rec eval ctx = function
Const b -> b
| Var s -> List.assoc s ctx
| Neg e -> not (eval ctx e)
| Disj (e1, e2) -> (eval ctx e1) || (eval ctx e2)
| Conj (e1, e2) -> (eval ctx e1) && (eval ctx e2)
| Cond (e1, e2) -> (not (eval ctx e1)) || (eval ctx e2)
| BiCond (e1, e2) -> (eval ctx e1) = (eval ctx e2);;

let rec prop_of_log_exp = function 
  Const c -> C c
  | Var s -> V s
  | Neg e -> Op (Not,prop_of_log_exp e)
  | Disj (e1,e2) -> BiOp (Or,prop_of_log_exp e1, prop_of_log_exp e2)
  | Conj (e1,e2) -> BiOp (And, prop_of_log_exp e1,prop_of_log_exp e2)
  | Cond (e1,e2) -> BiOp (If,prop_of_log_exp e1,prop_of_log_exp e2)
  | BiCond (e1,e2) -> BiOp (Iff,prop_of_log_exp e1,prop_of_log_exp e2);;

let rec log_exp_of_prop=function
  C c -> Const c
  | V s -> Var s
  | Op (Not,e)->Neg (log_exp_of_prop e)
  | BiOp (Or,e1,e2)-> Disj (log_exp_of_prop e1, log_exp_of_prop e2)
  | BiOp (And,e1,e2)-> Conj (log_exp_of_prop e1, log_exp_of_prop e2)
  | BiOp (If,e1,e2)-> Cond (log_exp_of_prop e1, log_exp_of_prop e2)
  | BiOp (Iff,e1,e2)-> BiCond (log_exp_of_prop e1, log_exp_of_prop e2);;

let opval = function
  Not -> not ;;

let biopval = function
  Or -> (||)
  | And -> (&&)
  | If -> (!=)
  | Iff -> (==);;

let rec peval ctx = function
  C c ->c
  | V s -> List.assoc s ctx
  | Op (Not,e)->opval Not (peval ctx e)
  | BiOp (Or,e1,e2)->biopval Or (peval ctx e1) (peval ctx e2)
  | BiOp (And,e1,e2)->biopval And (peval ctx e1) (peval ctx e2)
  | BiOp (If,e1,e2)->biopval If (peval ctx e1) (peval ctx e2)
  | BiOp (Iff,e1,e2)->biopval Iff (peval ctx e1) (peval ctx e2);;

(*TERMINAR*)
let rec is_tau p= if (peval [] p) then true else false;; 

(*Tenemos que crear la combinacion de todos los posibles valores  *)