type 'a g_tree = Gt of 'a * 'a g_tree list;;

let rec size = function 
    Gt (_,[]) -> 1
  | Gt (r,h::t) -> size h + size (Gt (r,t));;

(*TERMINAR*)
let rec height= function
  Gt (_,[])-> 1
  | Gt (r,h::t)->1+ max (height h) (height (Gt (r,t)));;

let rec mirror= function
  Gt(r,[])->Gt(r,[])
  | Gt(r,t)->Gt(r,(List.rev (List.map mirror t)));;

let rec preorden=function
  Gt(r,[])->[r]
  | Gt(r,(Gt(h,t1))::t)->[r]@ (preorden (Gt (h,(t1@t))));;

(*TERMINAR*)
let rec postorden=function
  Gt(r,[])->[r]
  | Gt(r,h::t)->[r]@ (List.fold_left postorden t);;
