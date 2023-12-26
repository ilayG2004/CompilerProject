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
  | Unit
  | Symbol of string
type com =
  | Push of const | Dup
  | Pop
  | Add | Sub | Mul | Div
  | Swap | Over | Rot
  | And | Or | Not
  | Lt | Gt| Eq
  | If of prog * prog
  | Jmp | Jz | Call | Ret
  | Bind | Lookup
  | Fun of prog
  | Trace
and prog = com list
type output = string list

(*  Eliminates whitespaces from charlist by building new charlist which ignores them  *)
let rec whitespace (xs : char list) : char list =
 let rec helper xs accum =
    match xs with
    | ' ' :: rest -> helper rest accum
    | '\t' :: rest -> helper rest accum
    | '\n' :: rest -> helper rest accum
    | '(' :: rest -> helper rest accum
    | ')' :: rest -> helper rest accum
    | ',' :: rest -> helper rest accum
    (*| '\f' :: rest -> helper rest accum*)
    (*| '\v' :: rest -> helper rest accum*)
    | c :: rest -> helper rest (c :: accum)
    | _ -> accum
  in List.rev(helper xs ([] : char list))

(*  Converts character to a digit by verifying it's numerical then subtracting its ASCII value from the ASCII value of zero *)
let ord = Char.code
let digit_of_char(ch: char): int =
  let () = assert(ch >= '0') in
    let () = assert(ch <= '9') in
      ord(ch) - ord('0')

(*  If our characters are digits, continue to accumulate them into an integer   *)
exception No_Proper_Closure
let rec parse_int accum = function
   | x :: xs when x = '-' -> -1 * parse_int 0 xs
   | x :: xs when x >= '0' && x <= '9' -> parse_int (10*accum + (digit_of_char x)) xs
   | x :: xs when x = ';' -> accum
   | xs -> raise No_Proper_Closure

(*  If our character is alphabetical, continue to accumulate into new string through appending accumulator to string of char x  *)
let rec parse_string accum = function
  | x :: xs when (x >= 'A' && x <= 'Z') || (x >= 'a' && x <= 'z') -> parse_string (accum ^ String.make 1 x) xs
  | x :: xs when x = ';' -> accum
  | xs -> raise No_Proper_Closure


(*  Reads characters to find correct Constant Type. If character does not relate to a constant, raise exception   *)
exception InvalidConst of char
let rec parse_const (xs : char list) : const =
  let rec helper xs =
    match xs with
    | 'U' :: 'n' :: 'i' :: 't' :: ';' :: rest -> (Unit)
    | 'T' :: 'r' :: 'u' :: 'e' :: ';' :: rest -> (Bool true)
    | 'F' :: 'a' :: 'l' :: 's' :: 'e' :: ';' :: rest -> (Bool false)
    | x :: rest -> 
      (if (x >= '0' && x <= '9') || (x = '-') then 
        let n = parse_int 0 (x :: rest) in
        (Int n)
      else if (x >= 'A' && x <= 'Z') || (x >= 'a' && x <= 'z') then
        let s = parse_string "" (x :: rest) in
        (Symbol s)
      else 
        raise (InvalidConst x)
      )
      in helper xs
  
(*  Parses our character list to check for valid commands, creates a program of these commands  *)
let rec parse_matcher (xs : char list) = 
  let ws = whitespace xs in (*Eliminate whietsapces so we can pattern match with ease through each character*)
  let rec helper xs accum = (*helper function which takes the character list & accumulator to build program*)
    match xs with
    | 'P' :: 'u' :: 's' :: 'h' :: rest -> helper rest ((Push (parse_const rest)) :: accum)
    | 'D' :: 'u' :: 'p' :: ';' :: rest ->        helper rest (Dup :: accum)
    | 'P' :: 'o' :: 'p' :: ';' :: rest ->        helper rest (Pop :: accum)

    | 'A' :: 'd' :: 'd' :: ';' :: rest -> helper rest (Add :: accum)
    | 'S' :: 'u' :: 'b' :: ';' :: rest -> helper rest (Sub :: accum)
    | 'M' :: 'u' :: 'l' :: ';' :: rest -> helper rest (Mul :: accum)
    | 'D' :: 'i' :: 'v' :: ';' :: rest -> helper rest (Div :: accum)

    | 'A' :: 'n' :: 'd' :: ';' :: rest ->  helper rest (And :: accum)
    | 'O' :: 'r' :: ';' :: rest ->         helper rest (Or :: accum)
    | 'N' :: 'o' :: 't' :: ';' :: rest ->  helper rest (Not :: accum)

    | 'L' :: 't' :: ';' :: rest ->         helper rest (Lt :: accum)
    | 'G' :: 't' :: ';' :: rest ->         helper rest (Gt :: accum)
    | 'E' :: 'q' :: ';' :: rest ->         helper rest (Eq :: accum)

    | 'J' :: 'm' :: 'p' :: ';' :: rest ->         helper rest (Jmp :: accum)
    | 'J' :: 'z' :: ';' :: rest ->                helper rest (Jz :: accum)
    | 'C' :: 'a' :: 'l' :: 'l' :: ';' :: rest ->  helper rest (Call :: accum)
    | 'R' :: 'e' :: 't' :: ';' :: rest ->         helper rest (Ret :: accum)

    | 'S' :: 'w' :: 'a' :: 'p' :: ';' :: rest -> helper rest (Swap :: accum)
    | 'O' :: 'v' :: 'e' :: 'r' :: ';' :: rest -> helper rest (Over :: accum)
    | 'R' :: 'o' :: 't' :: ';' :: rest ->        helper rest (Rot :: accum)

    | 'B' :: 'i' :: 'n' :: 'd' :: ';' :: rest ->                helper rest (Bind :: accum)
    | 'L' :: 'o' :: 'o' :: 'k' :: 'u' :: 'p' :: ';' :: rest ->  helper rest (Lookup :: accum)

    | 'I' :: 'f' :: rest -> (
      let c1, rest_after_else = extract_until_else rest in
      let c2, rest_after_end = extract_until_end rest_after_else in
      helper rest_after_end (If ((parse_matcher c1, parse_matcher c2)) :: accum)
    )
    | 'F' :: 'u' :: 'n' :: rest -> (
      let c1, rest_after_end = extract_until_end rest in
      helper rest_after_end ((Fun (parse_matcher c1)) :: accum)
    )

    | 'T' :: 'r' :: 'a' :: 'c' :: 'e' :: ';' :: rest -> helper rest (Trace :: accum)
    | [] -> accum
    | _  :: rest -> helper rest accum (*  Implement in future some form of compilation error here   *)
  in List.rev(helper ws ([] : prog))
and extract_until_end xs =
  let rec aux acc = function
    | 'E' :: 'n' :: 'd' :: ';' :: rest -> (List.rev acc, rest)
    | [] -> failwith "Unexpected end of input while parsing Fun"
    | c :: rest -> aux (c :: acc) rest
in aux [] xs
and extract_until_else xs =
  let rec aux acc = function
  | 'E' :: 'l' :: 's' :: 'e' :: rest -> (List.rev acc, rest)
  | [] -> failwith "Unexpected end of input while parsing If with Else"
  | c :: rest -> aux (c :: acc) rest
in aux [] xs


(*  Takes a string and return a character list made of its character  *)
let string_listize (s : string) : char list =
  let rec helper index acc =
    if index < 0 then
      acc
    else
      helper (index - 1) (s.[index] :: acc)
  in
  helper (String.length s - 1) []

(*  Parser which takes string from user and turns into stack language   *)
let parser (s : string) : prog = 
  parse_matcher(string_listize(s))


let push_com(curr_stack)(c)=
  curr_stack = (c :: curr_stack)

let dup_com (curr_stack)=
  match curr_stack with
  | x :: xs -> (x :: curr_stack)
  | [] -> failwith "Empty stack. Nothing to Duplicate"

let pop_com (curr_stack)=
  match curr_stack with
  | x :: xs -> xs (*Returns the stack w/o first element*)
  | [] -> failwith "Empty stack. Nothing to Pop"

let bopr_com (curr_stack)(opr)=
  match opr with
  | Add -> 
    (match curr_stack with
    | (Int x1) :: (Int x2) :: xs -> 
      let res = pop_com curr_stack in
      let res = pop_com curr_stack in
      push_com (res)(x1 + x2)
    | _ -> failwith "Add failure. Two integers do not exist at the top of the stack"
    )
  | Sub ->
    (match curr_stack with
    | (Int x1) :: (Int x2) :: xs -> 
      let res = pop_com curr_stack in
      let res = pop_com curr_stack in
      push_com (res)(x1 - x2)
    | _ -> failwith "Subtraction failure. Two integers do not exist at the top of the stack"
    )
  | Mul ->
    (match curr_stack with
    | (Int x1) :: (Int x2) :: xs -> 
      let res = pop_com curr_stack in
      let res = pop_com curr_stack in
      push_com (res)(x1 * x2)
    | _ -> failwith "Multiplication failure. Two integers do not exist at the top of the stack"
    )
  | Div ->
    (match curr_stack with
    | (Int x1) :: (Int x2) :: xs -> 
      let res = pop_com curr_stack in
      let res = pop_com curr_stack in
      push_com (res)(x1/x2)
    | _ -> failwith "Division failure. Two integers do not exist at the top of the stack"
    )
  | Lt -> 
    (match curr_stack with
    | (Int x1) :: (Int x2) :: xs -> 
      let res = pop_com curr_stack in
      let res = pop_com curr_stack in
      push_com (res)(x1 < x2)
    | _ -> failwith "Less than failure. Two integers do not exist at the top of the stack"
    )
  | Gt ->
    (match curr_stack with
    | (Int x1) :: (Int x2) :: xs -> 
      let res = pop_com curr_stack in
      let res = pop_com curr_stack in
      push_com (res)(x1 > x2)
    | _ -> failwith "Greater than failure. Two integers do not exist at the top of the stack"
    )
  | Eq -> 
    (match curr_stack with
    | (Int x1) :: (Int x2) :: xs -> 
      let res = pop_com curr_stack in
      let res = pop_com curr_stack in
      push_com (res)(x1 = x2)
    | _ -> failwith "Equal-to failure. Two integers do not exist at the top of the stack"
    )
  | And ->
    (match curr_stack with
    | (Bool x1) :: (Bool x2) :: xs -> 
      let res = pop_com curr_stack in
      let res = pop_com curr_stack in
      push_com (res)(x1 && x2)
    | _ -> failwith "And failure. Two booleans do not exist at the top of the stack"
    )
  | Or ->
    (match curr_stack with
    | (Bool x1) :: (Bool x2) :: xs -> 
      let res = pop_com curr_stack in
      let res = pop_com curr_stack in
      push_com (res)(x1 || x2)
    | _ -> failwith "Or failure. Two booleans do not exist at the top of the stack"
    )
  | Swap ->
    (match curr_stack with
    | x1 :: x2 :: xs -> 
      let res = pop_com curr_stack in
      let res = pop_com curr_stack in
      let res = push_com (res)(x1) in
      push_com (res)(x2)
    | _ -> failwith "Or failure. Two booleans do not exist at the top of the stack"
    )


(*  Runs whole stack program and returns end result   *)
let execute (s : string) : output
  let program = parser(s) in
  let rec helper program curr_stack =
    match program with
    | Push c :: rest -> helper rest (c :: curr_stack)
    | Dup :: rest -> helper rest (dup_com(curr_stack))
    | Pop :: rest -> helper rest (pop_com(curr_stack))
    | Add :: rest -> helper rest (bopr_com(curr_stack)(Add))
    | Sub :: rest -> helper rest (bopr_com(curr_stack)(Sub))
    | Mul :: rest -> helper rest (bopr_com(curr_stack)(Mul))
    | Div :: rest -> helper rest (bopr_com(curr_stack)(Div))
    | Swap :: rest -> -> helper rest (bopr_com(curr_stack)(Swap))
    | Over :: rest
    | Rot :: rest
    | And :: rest -> helper rest (bopr_com(curr_stack)(And))
    | Or  :: rest -> helper rest (bopr_com(curr_stack)(Or))
    | Not :: rest
    | Lt :: rest -> helper rest (bopr_com(curr_stack)(Lt))
    | Gt :: rest -> helper rest (bopr_com(curr_stack)(Gt))
    | Eq :: rest -> helper rest (bopr_com(curr_stack)(Eq))
    | If(c1, c2) :: rest
    | Jmp :: rest
    | Jz :: rest
    | Call :: rest
    | Ret :: rest
    | Bind :: rest
    | Lookup :: rest
    | Fun(c1) :: rest
    | Trace :: rest
  in helper program []