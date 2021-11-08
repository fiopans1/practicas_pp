(*DIEGO SUAREZ RAMOS- diego.suarez.ramos@udc.es*)
let rec fact = function
0 -> 1
| n -> n * fact (n - 1);;

if(Array.length(Sys.argv)<>2)
then
print_string "fact invalido"
else begin
print_int(fact(int_of_string(Sys.argv.(1))));
print_string "\n"
end