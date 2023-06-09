(** Basic arithmetics with built-in integers *)

open Builtin

(* Greater common divisor and smaller common multiple
   implemetations.
*)

(** Greater common (positive) divisor of two non-zero integers.
    @param a non-zero integers
    @param b non-zero integer
*)

let rec gcd a b = if b = 0 then a 
else
gcd b (modulo a b);;


(* Extended Euclidean algorithm. Computing Bezout Coefficients. *)

(** Extended euclidean division of two integers NOT OCAML DEFAULT.
    Given non-zero entries a b computes triple (u, v, d) such that
    a*u + b*v = d and d is gcd of a and b.
    @param a non-zero integer
    @param b non-zero integer.
*)
let bezout a b = let rec bezou(u,f,g,u',f',g')=
                      if g' = 0 then
                         (u,f,g)
                       else
let v = g/g' in
    bezou(u',f',g',u-v*u',f-v*f',g-v*g')
in bezou(1,0,a,0,1,b);;


