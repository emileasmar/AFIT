(** Generating primes *)

open Builtin
open Basic_arithmetics
open Printf 


(* Initializing list of integers for eratosthenes's sieve. Naive
   version.
*)

(** List composed of 2 and then odd integers starting at 3.
    @param n number of elements in the list of integers.
 *)
let init_eratosthenes n = 
    let rec eratos(m,n,l) =
       if m > n then 
         l
        else 
           eratos(m+2,n,m::l)
      in eratos(3,n,[2]) ;;

(* Eratosthenes sieve. *)

(** Eratosthene sieve.
    @param n limit of list of primes, starting at 2.
*)
let is_prime x = 
  let nbdiv x = let rec nb x a b = 
    if a = 1 then b
    else
      if (x mod a = 0) then nb x (a-1) (b+1)
      else 
        nb x (a-1) b in nb x x 1
in  nbdiv x=2 ;;

let eratosthenes n = 
   let rec eratos n i = match i with
   i when i > n -> []
 | _ -> if is_prime i then i::(eratos n (i+1)) else eratos n (i+1)
in eratos n 2 ;;


(* Write and read into file functions for lists. *)

(** Write a list into a file. Element seperator is newline.
    @param file path to write to.
 *)
let write_list li file =(
   let rec write_list1 chan li = match li with
    e::nli -> fprintf chan  e;  
  | [] -> ()
in
let oo = open_out file 
 in 
   begin 
      write_list1 oo li;
      close_out oo;
    end );;

(** Write a list of prime numbers up to limit into a txt file.
    @param n limit of prime numbers up to which to build up a list of primes.
    @param file path to write to.
*)
let write_list_primes n file = ()

(** Read file safely ; catch End_of_file exception.
    @param in_c input channel.
 *)
let input_line_opt in_c =
  try Some (input_line in_c)
  with End_of_file -> None

(** Create a list out of reading a line per line channel.
    @param in_c input channel.
 *)
let create_list in_c = ()

(** Load list of primes into OCaml environment.
    @param file path to load from.
 *)
let read_list_primes file = []

(* Auxiliary functions to extract big prime numbers for testing
   purposes.
 *)

(** Get biggest prime.
    @param l list of prime numbers.
 *)
let rec last_element l = match l with
  | [] -> failwith "Builtin.generate_primes.last_element: Your list \
                    is empty. "
  | e::[] -> e
  | h::t -> last_element t

(** Get two biggest primes.
    @param l list of prime numbers.
 *)
let rec last_two l = match l with
  | [] | [_] -> failwith "Builtin.generate_primes.last_two: List has \
                          to have at least two prime numbers."
  | e::g::[] -> (e, g)
  | h::t -> last_two t
;;

(* Generating couples of prime numbers for specific or fun
   purposes.
 *)

(** Finding couples of primes where second entry is twice the first
    plus 1.
    @param limit positive integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)
let double_primes limit isprime = ( 
   let rec double li = match li with
  [] -> []
| e::nli when (is_prime e) && (is_prime (e*2+1)) ->(e, e*2+1)::(double nli)
| e::nli -> (double nli) 
in
 double (eratosthenes limit) );;
(** Finding twin primes.
    @param limit positive integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)
let twin_primes limit isprime = ( 
   let rec find_twins li =
     match li with 
       |[] -> []
       |e::nli when (isprime e) && (isprime (e+2)) -> (e, e+2)::(find_twins nli)
       |e::nli -> (find_twins nli)
in 
   if (limit > 3) then (2,3)::find_twins (eratosthenes limit) 
   else
   find_twins (eratosthenes limit) );;
 
