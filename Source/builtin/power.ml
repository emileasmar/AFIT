(** Power function implementations for built-in integers *)

open Builtin
open Basic_arithmetics

(* Naive and fast exponentiation ; already implemented in-class.
 *)

(** Naive power function. Linear complexity
    @param x base
    @param n exponent
 *)
let rec pow x n = 
    if n = 0 then 1
    else
    x * ( pow x (n-1)) ;;

(** Fast integer exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
 *)
let power x n = match n with
n when n mod 2 = 0 -> (pow (pow x (n/2))2)
|_ -> (pow (pow x (n/2))2) * x ;;

(* Modular expnonentiation ; modulo a given natural number smaller
   max_int we never have integer-overflows if implemented properly.
 *)

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
    @param m modular base
 *)
let mod_power x n m = let rec modpower nb o p=
                         if o = 0 then 1
                         else
                         if nb = 0 then 0 
                         else
let a = modulo (modpower nb (o/2) p) p in
let b = modulo (a*a) p in
let c = match (modulo o 2) with
      0 -> b
    | _ -> modulo (b * nb) p
in modulo (c) p
in modpower (modulo x m) n m ;;


(* Making use of Fermat Little Theorem for very quick exponentation
   modulo prime number.
 *)

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base
    @param n exponent
    @param p prime modular base
 *)
let prime_mod_power x n p = 
   let rec prime t n =(match n with
 0 -> 1
| 1 -> t
| x when x = p-1 -> 1
| x -> let y = prime t (x/2) in modulo ( y * y * (if x mod 2 = 0 then 1 else t)) p)
in modulo (prime x n) p;;

              

