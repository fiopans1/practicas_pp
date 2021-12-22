let rec divide = function
  [] -> [], []
  | h::[] -> [h], []
  | h1::h2::t -> let t1, t2 = divide t in (h1::t1, h2::t2);;

let rec merge ord = function
  [], l | l, [] -> l
  | h1::t1, h2::t2 -> if (ord h1) h2 then h1 :: merge ord (t1, h2::t2)
                      else h2 :: merge ord (h1::t1, t2);;

let rec msort1 ord l = match l with
  [] | _::[] -> l
  | _ -> let l1, l2 = divide l in
        merge ord (msort1 ord l1, msort1 ord l2);;





let rec merge ord v izq der cent=
  let i= (ref izq);let j= (ref (cent+1)); k=(ref izq); let aux=(Array.make (der+1) 0) in
while i<= cent && j<= der do (
    if (ord v.((!i)) v.((!j))) then (aux.((!k)) <- v.((!i)) ; i := (!i + 1))
    else aux.((!k)) <- v.((!j)) ; j = (!j + 1) ; k := (!k +1))
done ;
while !i <= cent do
  aux.((!k)) <- v.((!i)) ; i := (!i +1) ; k := (!k +1)
let rec merge ord v izq der cent=
  let i= (ref izq);let j= (ref (cent+1)); k=(ref izq); let aux=(Array.make (der+1) 0) in
while i<= cent && j<= der do (
    if (ord v.((!i)) v.((!j))) then (aux.((!k)) <- v.((!i)) ; i := (!i + 1))
    else aux.((!k)) <- v.((!j)) ; j = (!j + 1) ; k := (!k +1))
done ;

while !i <= cent do
  aux.((!k)) <- v.((!i)) ; i := (!i + 1) ; k := (!k +1))
done ;
while !i <= der do
  aux.((!k)) <- v.((!j)) ; j := (!j + 1) ; k := (!k +1))
done ;
for k:=izq to der do
  v.(k) <- aux.(k)
done ;
 () ;;



(* Merge is written according to the pseudocode in "Introduction to Algorithms" 3rd edition p.31. a is the array, p q r are the number of element in the array, therefore, counting starts from 1, not 0.*)
let merge a p q r =
  let n1 = ref (q - p + 1) and n2 = ref (r - q) in
    let la = Array.make (!n1 + 1) max_int and ra = Array.make (!n2 + 1) max_int in
      for i = 0 to !n1 - 1 do
        la.(i) <- a.(p + i - 1)
      done;
      for j = 0 to !n2 - 1 do
        ra.(j) <- a.(q + j)
      done;
        let i = ref 0 and j = ref 0 in
          for k = p - 1 to r - 1 do
            if la.(!i) <= ra.(!j) then
              (a.(k) <- la.(!i) ;
              i := !i + 1)  
            else 
              (a.(k) <- ra.(!j) ; 
              j := !j + 1) 
          done
;;

(* merge_sort is written following the pseudocode on p.34 of the same book, using merge as a subroutine. a is the array, p = 1 (first element) r = length of array. *)
let rec merge_sort a p r =
  let c = ref r in
    while p < r && p < !c do
      merge_sort a p ((p + r)/2);
      merge_sort a ((p + r)/2 + 1) r;
      merge a p ((p + r)/2) r;
      c := (!c + p)/2
    done
;;