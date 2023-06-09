(** Factoring Built-In Int Primes *)

open Builtin
open Basic_arithmetics

(** Factors product of two primes.
    @param key is public key of an RSA cryptosystem.
 *)
let break (key,nouse) =
    let rec break1 key d = if modulo key d = 0 
                           then d 
                           else
                           break1 key (d-1)
                        in 

let rec break2 key k = 
if modulo key k = 0 then k 
else
break2 key (k-1)
in (break2 key ((break1 key (key-1))-1),
   break1 key (key-1));;
