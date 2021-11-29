(*DIEGO SUAREZ RAMOS- diego.suarez.ramos@udc.es*)
let nofuevisitado l e=
  not(List.mem e l);;

let limit m n (x,y)=
  (x>=1 && x<=m && y>=1 && y<=n);;
let movimientolegal m n (x,y) visits =
  let todoslosmovimientos = [x+1,y+2; x+2,y+1; x-1,y+2; x-2,y+1;
      x+1,y-2; x+2,y-1; x-1,y-2; x-2,y-2]
in List.filter (nofuevisitado visits) (List.filter (limit m n) todoslosmovimientos);;
let rec tour m n (i,j) (x,y)=
  if(not(limit m n (i,j)) || not(limit m n (x,y))) then
      raise(Invalid_argument "tour") else
      let rec aux l= function
      []->raise(Not_found)
      | h::t-> if(h=(x,y))
        then List.rev(h::l)
        else try aux (h::l) (movimientolegal m n h (h::l))
      with Not_found->aux l t
    in if (limit m n (i,j)) then (aux [] [i,j])
    else raise(Not_found);;
      |
