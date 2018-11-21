(* sortedlist.ml : Provides operations for sorted lists of any type. *)
open Printf;;
let rec insert list elem =
  match list with
  | [] -> [elem]  (*empty list*)
  | a :: [] when elem > a -> a :: elem :: [] (*tacking elem on the end of list*)
  | a :: [] when (elem = a) -> [a]  (*keep list as is for duplicate*)
  | head :: tail when elem < head -> (elem :: list) (*found insertion point*)
  | head :: tail when elem = head -> list (*keep list as is*)
  | head :: tail -> head :: insert tail elem (*didnt find spot, recurse on the tail*)
;;

let rec remove list elem =
  match list with
  | [] -> []  (*returns emptylist in case where list is empty/element is not found*)
  | a :: b when elem = a -> b (*case where elem  is found*)
  | a :: b -> (a :: remove b elem) (*case where elem is not found and we havent reaced the end*)
;;

let rec print strlist =
  match strlist with
  | [] ->  ()     (*returns unit in empty list case*)
  | a :: b -> printf "%s\n" a; print b  (*prints head and recurses on tail*)
;;
let rec merge lista listb =
  match lista,listb with
  | [],[] -> []                                  (*empty list case*)
  | [],(bh::bt) -> listb                         (*cases where there is an empty*)
  | (ah::at),[] -> lista                         (*list*)

  | (a::[]),(b::[]) ->                           (*case where both lists have one element*)
    let heada = List.hd lista in
    let headb = List.hd listb in
    if heada > headb then
      headb :: lista
    else if heada = headb then
      [heada]
    else
      heada :: listb
  | (ah::at),(bh::bt) when ah = bh -> ah :: merge at bt  (*case where elements match*)
  | (ah::at),(bh::bt) when ah < bh -> (ah :: merge at listb)(*case where a element is smaller*)
  | (ah::at),(bh::bt) when ah > bh -> (bh :: merge lista bt)(*case where b element is smaller*)
 ;;
