(* subset a b that returns true iff a⊆b, i.e., if the set represented 
 * by the list a is a subset of the set represented by the list b. *)
let rec contains e l =
    match l with
    | [] -> false
    | h::t -> if e = h then true else  contains e t

let rec subset a b = 
    match a with 
    | [] -> true 
    | h::t -> if (contains h b) then  subset t b else false

(* equal_sets a b returns true iff the represented sets are equal.*)
let equal_sets a b = 
    if (subset a b && subset b a) then true else false

(* set_union a b returns a list representing a∪b.*)
let rec set_union a b =
    match a with 
    | [] -> b 
    | h::t -> h::( set_union t b)

(* set_intersection a b returns a list representing a∩b.*)
let rec set_intersection a b = 
    match a with
    | [] -> []
    | h::t -> if(contains h b) then [h]@(set_intersection t b)
              else (set_intersection t b)

(* set_diff a b returns a list representing a−b, that is, the set of 
 * all members of a that are not also members of b.*)
let rec set_diff a b = 
    match a with
    | [] -> []
    | h::t -> if(contains h b) then (set_diff t b)
              else h::(set_diff t b)

(* computed_fixed_point eq f x that returns the computed fixed point 
 * for f with respect to x *)
let rec computed_fixed_point eq f x =
    let a = f x in
    if ( eq a x) then x
    else computed_fixed_point eq f a

let rec compute_f f p x = 
    if (p = 0) then x
    else
        compute_f f (p-1) (f x)

let rec computed_periodic_point eq f p x = 
    if (p == 0) then x
    else 
        let a = compute_f f p x in
        if ( eq a x) then a 
        else computed_periodic_point eq f p (f x) 


type ('nonterminal, 'terminal) symbol =
      | N of 'nonterminal
      | T of 'terminal

(* check if a symbol is in the rules list ('a * 'b) *)
let rec isContain s t_list = 
    match t_list with
    | [] -> false 
    | h::t -> match h with | (ex, l) -> if(s == ex) then true else (isContain s t)

(* return true if rhs is terminal symbol,
 *          it is a terminal symbol 
 *              if rule contains all terminal symble
 *              if rule contains all terminal symble + terminal rules
 *              *)
let rec checkTerminalSymbol rhs t_list = 
    match rhs with
    | [] -> true
    | (T _)::t ->  (checkTerminalSymbol t t_list)
    | (N r)::t -> if(isContain r t_list) then (checkTerminalSymbol t t_list) else false

(*
 * isolating terminal rules everytime the function has called
 * *)
let rec filter t_rules n_rules = 
    match n_rules with
    | [] -> t_rules
    | h::tail -> match h with 
            | (lhs, rhs) -> if(checkTerminalSymbol rhs t_rules) then (filter (t_rules@[h]) tail) else (filter t_rules tail)
   
(*
 * isolating terminal rules recursively until there are no more rules can be separated
 * *)
let rec filter_rules t_rules n_rules = 
    let terminalList = filter t_rules (set_diff n_rules t_rules) in
        if(List.length terminalList == List.length t_rules) then terminalList else (filter_rules terminalList n_rules)

let reorderRules goodRules awksub_rules = 
    set_intersection goodRules awksub_rules

let rec unionRHS teminateRules = 
    match teminateRules with 
    | [] -> []
    | (lhs, rhs)::t -> (set_union rhs (unionRHS t))

let rec removeUnusedRules goodRules goodSymbols = 
    match goodRules with
    | [] -> []
    | h::t -> match h with | (lhs, rhs) -> if(contains (N lhs) goodSymbols) then (set_union [h] (removeUnusedRules t goodSymbols)) 
                                                                    else (removeUnusedRules t goodSymbols)

let filter_blind_alleys g = 
    match g with
    | (expr, awksub_rules) -> let goodRules = (filter_rules [] awksub_rules) in
                                    let goodSymbols = unionRHS goodRules in
                                    (expr, reorderRules awksub_rules (removeUnusedRules goodRules (set_union [(N expr)] goodSymbols)))
                                    
