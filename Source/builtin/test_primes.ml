(** Testing for primality *)

open Builtin
open Basic_arithmetics
open Power

(** Deterministic primality test *)
let is_prime n = 
   let div n = let rec nb n a b =
     if a = 1 then b
     else
       if (n mod a = 0) then nb n (a-1) (b+1)
       else 
         nb n (a-1) b in nb n n 1
in div n = 2 ;; 

(** Pseudo-primality test based on Fermat's Little Theorem
    @param p tested integer
    @param testSeq sequence of integers againt which to test
 *)
let is_pseudo_prime p test_seq = 
   let rec pseudo p test_seq = match test_seq with
   [] -> true
 | e::test_seq when ((gcd p e) = 1) || ((modulo e p) = 0) -> pseudo p test_seq
 | _ -> false
in pseudo p test_seq ;;

