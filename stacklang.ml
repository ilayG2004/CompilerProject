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

(*Formal grammars defined as types in OCaml*)
type const =
  | Int of int
  | Bool of bool
  | Unit of ()
  | Symbol of string
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

(*  Eliminates whitespaces from charlist by building new charlist which ignores them  *)
let rec whitespace (xs : char list) : char list =
 let rec helper xs accum =
    match xs with
    | ' ' :: rest -> helper rest accum
    | '\t' :: rest -> helper rest accum
    | '\n' :: rest -> helper rest accum
    | '\f' :: rest -> helper rest accum
    | '\v' :: rest -> helper rest accum
    | c :: rest -> helper rest (c :: accum)
  in helper xs ([] : char list)

(*  Converts character to a digit by verifying it's numerical then subtracting its ASCII value from the ASCII value of zero *)
let ord = Char.code
let digit_of_char(ch: char): int =
  let () = assert(ch >= '0') in
    let () = assert(ch <= '9') in
      ord(ch) - ord('0')

(*  If our characters are digits, continue to accumulate them into an integer   *)
let rec parse_int accum = function
   | x :: xs when x >= '0' && x <= '9' -> parse_int (10*accum + (digit_of_char x)) xs
   | xs -> accum

(*  If our character is alphabetical, continue to accumulate into new string through appending accumulator to string of char x  *)
let rec parse_string accum = function
  | x :: xs when (x >= 'A' && x <= 'Z') || (x >= 'a' && x <= 'z') -> parse_string (accum ^ Str.make x) xs
  | xs -> accum

let rec parse_const (xs : char list) : const =
  let rec helper xs accum =
  | 'U' :: 'n' :: 'i' :: 't' :: rest -> (Unit ())
  | 'T' :: 'r' :: 'u' :: 'e' :: rest -> (Bool true)
  | 'F' :: 'a' :: 'l' :: 's' :: 'e' :: rest -> (Bool false)
  | x :: rest -> 
    if x >= '0' && x <= '9' then 
      let n = parse_int 0 (x :: rest) in
      (Int n)
    if (x >= 'A' && x <= 'Z') || (x >= 'a' && x <= 'z') then
      let s = parse_string "" (x :: rest) in
      (Symbol s)
  

let rec parse_matcher (xs : char list) = 
  let rec helper xs accum = #helper function which takes the character list & accumulator to build program
  match xs with
  | 'P' :: 'u' :: 's' :: 'h' :: rest -> helper rest ((Push (parse_const rest)) :: accum)
  | 'P' :: 'o' :: 'p' :: rest ->        helper rest (Pop :: accum)

  | 'A' :: 'd' :: 'd' :: rest -> helper rest (Add :: accum)
  | 'S' :: 'u' :: 'b' :: rest -> helper rest (Sub :: accum)
  | 'M' :: 'u' :: 'l' :: rest -> helper rest (Mul :: accum)
  | 'D' :: 'i' :: 'v' :: rest -> helper rest (Div :: accum)

  | 'A' :: 'n' :: 'd' :: rest ->  helper rest (And :: accum)
  | 'O' :: 'r' :: rest ->         helper rest (Or :: accum)
  | 'N' :: 'o' :: 't' :: rest ->  helper rest (Not :: accum)

  | 'L' :: 't' :: rest ->         helper rest (Lt :: accum)
  | 'G' :: 't' :: rest ->         helper rest (Gt :: accum)
  | 'E' :: 'q' :: rest ->         helper rest (Eq :: accum)
  | 'N' :: 'e' :: 'q' :: rest ->  helper rest (Neq :: accum)

  | 'J' :: 'm' :: 'p' :: rest ->  helper rest (Jmp :: accum)
  | 'J' :: 'z' :: rest ->         helper rest (Jz :: accum)
  | 'C' :: 'a' :: 'l' :: 'l' :: rest -> helper rest (Call :: accum)
  | 'R' :: 'e' :: 't' :: rest ->  helper rest (Ret :: accum)

  | 'S' :: 'w' :: 'a' :: 'p' :: rest -> helper rest (Swap :: accum)
  | 'O' :: 'v' :: 'e' :: 'r' :: rest -> helper rest (Over :: accum)
  | 'R' :: 'o' :: 't' :: rest ->        helper rest (Rot :: accum)
  in helper xs ([] : prog)

(*Parser which takes string from user and turns into stack language*)
let parser (s : string) : prog = 
