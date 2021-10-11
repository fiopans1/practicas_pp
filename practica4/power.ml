(*DIEGO SUAREZ RAMOS - diego.suarez.ramos@udc.es*)
let rec power x y=
	if y=0 then 1 else x*((power x) (y-1));;


(*ahora definimos la funcion de manera eficiente, y es mas eficiente porque hacemos la mitad de llamadas a la pila, lo que significa que la pila aguantará más sin llenarse, porque al ser recursividad no terminal podemos tener stack overflow, y con la segunda llamada, podemos tener el doble de valores que en la primera, aunque de poco sirve aunque al estar operando en int y y no Z, llegara un punto en el que todos los power dean 0 o se reseten los valores al pasar de int_max al estar trabajando es un espacio modular así que tampoco tiene mucho sentido*)
let rec par x z=if z=0 then 1 else (x*x)*((par x) (z-1));;
let rec impar x z= if z=0 then 1 else x*(x*x)*((impar x) (z-1));;

let power' x y= 
	let n=y/2 in
	if (y mod 2)= 0 then ((par x) n) else ((impar x) n);;
	
