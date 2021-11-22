(*curry : ((’a * ’b) -> ’c) -> (’a -> (’b -> ’c))*)
let curry f=fun a b-> (f (a,b));;

let f a,b= 0;;

let uncurry f=
  fun (a,b)->(f a) b;;

