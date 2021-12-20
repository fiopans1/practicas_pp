type 'a g_tree = Gt of 'a * 'a g_tree list;;

let rec size = function 
    Gt (_,[]) -> 1
  | Gt (r,h::t) -> size h + size (Gt (r,t));;

  let rec height =
    function
    | Gt (_, []) -> 1
    | Gt (_, _::t) -> 2+ List.fold_left max 0 (List.map height t);;
    (*en esta funcion el fold_left lo que hace es sumar todos los valores
    de una lista, y le pasas como variable la lista de alturas(la lista de nodos
    calculandole la altura*)

let rec mirror= function
  Gt(r,[])->Gt(r,[])
  | Gt(r,t)->Gt(r,(List.rev (List.map mirror t)));;

let rec preorden=function
  Gt(r,[])->[r]
  | Gt(r,(Gt(h,t1))::t)->[r]@ (preorden (Gt (h,(t1@t))));;

(*TERMINAR*)
let rec postorden=function
  Gt(r,[])->[r]
  | Gt(r,h::t)-> (postorden h)  @ (postorden (Gt (r,t)));;



      