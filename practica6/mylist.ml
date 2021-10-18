(*DIEGO SUAREZ RAMOS- diego.suarez.ramos@udc.es*)
let hd l= match l with
  []->raise(Failure "hd")
  | h::_->h;;

(************************************************************)
let tl l= match l with
  []->raise(Failure "tl")
  | _::t->t;;
(************************************************************)
let rec len l n= match l with
    []->n
    | _::t-> len t (n+1);;

let length l=
        len l 0;;
(************************************************************)
let compare_lengths l1 l2=
  let (x,y)=((length l1), (length l2)) in
    if x>y then 1 else (if x<y then -1 else 0);;
(************************************************************)
let rec nthi l n p= 
  match l with
      []->raise(Failure "nth")
      | h::[]-> h
      | h::t -> if( p=n) then h else (nthi t n (p+1));;
let nth l n= 
    if(n<length l) then (nthi l n 0) else raise(Failure "nth");;
(**************************************************************)
let rec append l1 l2= match l1 with
  []->l2
  | h::t -> h::(append t l2);;
(**************************************************************)
let rec find f l= match l with
    []->raise(Failure "Ningun elemento cumple")
    | h::t-> if(f h) then h else find f t;;
(**************************************************************)
let rec for_alli f l= match l with
    []->true
    | h::t-> if(f h) then (for_alli f t) else false;;
let for_all f l= match l with
    []->raise Not_found
    | h::t-> (for_alli f l);;
(**************************************************************)
let rec exists f l= match l with
    []->false
    | h::t-> if(f h) then true else (exists f t);;
(**************************************************[*]***********)
let rec mem n l= match [l] with
  []->false
  | h::t->if(h=n) then true else (mem n t);;
(**************************************************************)
let rec fil f l l1= match l with
  []->l1
  | h::t->if(f h) then (fil f t (append l1 [h])) else (fil f t l1);;
let filter f l= (fil f l []);;
(**************************************************************)
let find_all f l= filter f l;;
(**************************************************************)
let rec parti f l l1 l2= match l with
  []->(l1,l2)
  | h::t-> if(f h) then (parti f t (append l1 [h]) l2) else (parti f t l1 (append l2 [h]));;
let partition f l= match l with
  []->raise(Failure "partition")
  | h::t-> parti f l [] [];;
