(** Ciphers
    Built-in integer based ciphers.
*)

open Builtin
open Basic_arithmetics
open Power

(********** Cesar Cipher **********)

(** Cesar's cipher encryption
    @param k is an integer corresponding to key
    @param m word to cipher.
    @param b base ; for ASCII codes should be set to 255.
 *)
let encrypt_cesar k m b = let rec cesar l = match l with
x::l -> (modulo (k+x) b)::(cesar l)
| [] -> []
in cesar m ;;

(** Cesar's cipher decryption
    @param k is an integer corresponding to key
    @param m encrypted word.
    @param b base ; for ASCII code should be set to 255.
 *)
let decrypt_cesar k m b = let rec cesar d = match d with
|[] -> []
|x::d -> (modulo(k-x) b)::(cesar d)
in cesar m ;;

(********** RSA Cipher **********)

(** Generate an RSA ciphering keys.
    Involved prime numbers need to be distinct. Output is a couple
    of public, private keys.
    @param p prime number
    @param q prime number
*)
let generate_keys_rsa p q = let cph = (p-1)*(q-1) and n = p*q 
in
let rec gen1 e = if (gcd e cph = 1) then e else gen1 (e+1)
in
let e = gen1 (2) 
in
let rec gen2 f = match f with
|_ when (modulo (e*f) cph = 1) -> f
|_ -> gen2 (f+1)
in
((n,e),(n,gen2 (2)));;


(** Encryption using RSA cryptosystem.
    @param m integer hash of message
    @param pub_key a tuple (n, e) composing public key of RSA cryptosystem.
 *)

let encrypt_rsa m (n, e) = 
mod_power m e n ;;

(** Decryption using RSA cryptosystem.
    @param m integer hash of encrypter message.
    @param pub_key a tuple (n, d) composing private key of RSA cryptosystem.
 *)

let decrypt_rsa m (n , d) = 
mod_power m d n ;;

(********** ElGamal Cipher **********)

(** Generate ElGamal public data. Generates a couple (g, p)
    where p is prime and g having high enough order modulo p.
    @param p is prime having form 2*q + 1 for prime q.
 *)
let rec public_data_g p = let g = Random.int p in match g with
|x when mod_power x p p <> 1 -> (g,p)
|_ -> public_data_g p ;;


(** Generate ElGamal public data.
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let generate_keys_g (g, p) = 
    let x = 1 + Random.int (p-1) in
    let y = mod_power g x p
in (y,x) ;;

(** ElGamal encryption process.
    @param msg message to be encrypted.
    @param pub_data a tuple (g, p) of ElGamal public data.
    @param kA ElGamal public key.
 *)
let encrypt_g msg (g, p) kA = 
    let k = 1 + ((Random.int(1000) + 1) / 2)
    in  (mod_power g k p , modulo (msg*(mod_power kA k p)) p) ;;

(** ElGamal decryption process.
    @param msg a tuple (msgA, msgB) forming an encrypted ElGamal message.
    @param a private key
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let decrypt_g (msgA, msgB) a (g, p) = 
    let (x,_,_) = bezout msgA p in
    modulo (msgB * mod_power (modulo x p) a p) p ;;
