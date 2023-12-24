(*Constants
   <digit> ::= 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
   <nat> ::= <digit> | <digit><nat>
   <int> ::= <nat> | -<nat>
   <bool> ::= True | False
   <const> ::= <int> | <bool> | Unit

  Programs
   <prog> ::= <coms>
   <com> ::= Push <const> | Pop | Trace
            | Add | Sub | Mul | Div
            | And | Or | Not
            | Lt | Gt
   <coms> ::= "" | <com>; <coms>

*)

(*Use regular list to implement stack itself, but input to interpreter is string*)

type const =
  | Int of int
  | Bool of bool
  | Unit of ()
type com =
  | Push of const | Dup
  | Pop
  | Add | Sub | Mul | Div
  | And | Or | Not
  | Lt | Gt| Eq | Neq
  | If of coms * coms
  | Jmp | Jz | Call | Ret
  | Swap | Over | Rot
and prog = com list

let rec whitespace (xs : char list) : char list =
 let rec helper xs accum = #helper function which takes the character list & accumulator to build program
    match xs with
    | ' ' :: rest -> helper rest accum
    | '\t' :: rest -> helper rest accum
    | '\n' :: rest -> helper rest accum
    | '\f' :: rest -> helper rest accum
    | '\v' :: rest -> helper rest accum
    | c :: rest -> helper rest (c :: accum)
  in helper xs ([] : char list)

let parse_matcher (xs : char list) = 
  let rec helper xs accum = #helper function which takes the character list & accumulator to build program
  match xs with
  
  in helper xs ([] : prog)

(*Parser which takes string from user and turns into stack language*)
let parser (s : string) : prog = 
