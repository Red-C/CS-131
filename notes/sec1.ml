let dummy = 5

(* 
 * Cons ::
    (element of list)::(tail of the list)

    tail of list is of type list: 'a list
    1::2::3::[] = [1;2;3]
    :: has an element on the left and list on right
    :: is right associative
    1::2 is invalid since 2 is not of type int list
    {'a;'a..... ; 'a} = 'a :: 'a :: .... :: []

Append @

Pattern Matching 
    let dummy arg1 arg2 ... =..
        match arg1 with
        |   [] -> do somthing
        |   [1;2] -> matches lists that are exactly [1;2]
        |   [h;;t] -> matches a list of lists, with at least 1 element in the outer list
        |   [x;y] -> x::y::[]*
        |   x::xs -> matches a list of head x and tail xs
        |   _::t -> matches a list with tail t, don't care about first element**
        |   h::m::t -> a list of some length with two single elements at leas
        |   (x,y) -> matches tuples of two element*
        |   (_,y) ->matches tuples, don't care about first elements*
        |   x when x ==3 -> matches element x when x is equal to 3*
        |   (x,y)::tail -> matches list of tuples of two elements
        |   _ -> matches everything
.
*
* 
* *)
(*
 * input [] -> []
 * input [2] -> []
 *)
let rec everySecond l =
    match l with
    | a::b::tail -> b:: (everySecond tail)
    | [] -> []
    | x::[] -> []

  (* 
   * [(1,2);(3,4)] ->[2;4]
   * [] -> []
   * [(1,2)] ->[2]
   * *)
let rec secondGet l = 
    match l with 
    | [] -> []
    | (x,y)::t -> y::( secondGet t)

(*
 * check if element e is in list l -> if it is return true
 *                      otherwise false
 *)

let rec contains e l = 
    match l with
    | [] -> false 
    | h::t -> if h == e then
            true
        else
            contains e t

(* range 1 3 1
 *)

let rec range from till step =
    if from > till then
        []
    else from::range (from+step) till step

(*
 * Convert a list to indexd tuples
 * ["a";"b";"c"] -> (1,"a");(2,"b");(3,"c")
 *)

let rec indexedList l=
    let rec indexdedListInner index l=
        match l with
        | [] -> []
        | a::tail -> (index,a)::indexdedListInner (index+1) tail in
    indexdedListInner 1 l
(*
 * check if list l1 is a prefix of l2 -> if ye return true
 * else false
 *)
let rec prefixOf l1 l2 =
    match l1 with 
    | [] -> true
    | x::tail -> match l2 with
                 | [] -> false
                 | y::l2tail-> if x == y then
                                    prefixOf tail l2tail 
                 else
                     false
           
let rec prefixOf l1 l2 = 
    match (l1,l2) with
    | [],_ -> true
    | _,[] -> false
    | x::xs,y::ys -> if x==y then 
                         prefixOf xs ys
                     else
                         false
