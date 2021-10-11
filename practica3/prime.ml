(*DIEGO SUAREZ RAMOS- diego.suarez.ramos@udc.es*)
let is_prime n =
let rec check_from i =
i >= n ||
(n mod i <> 0 && check_from (i+1))
in check_from 2;;

(*let is_prime2 n =
let rec check_from i =
i >= sqrt(n) ||
(n mod. i <> 0 && check_from (i+1))
in check_from 2;;*)


let rec next_prime z = let is_prime n =
let rec check_from i =
i >= n ||
(n mod i <> 0 && check_from (i+1))
in check_from 2 in if is_prime(z)=true then print_int(z) else next_prime(z+1);;

let rec last_prime_to z = let is_prime n =
let rec check_from i =
i >= n ||
(n mod i <> 0 && check_from (i+1))
in check_from 2 in if (is_prime(z)=true && z>0) then print_int(z) else last_prime_to(z-1);;
