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
  | Closure of (string * ((string * const) list) * prog)
and com =
  | Push of const | Dup
  | Pop
  | Add | Sub | Mul | Div
  | Swap | Over
  | And | Or | Not
  | Lt | Gt| Eq
  | If of prog * prog
  | Jmp | Jz | Call | Ret
  | Bind | Lookup
  | Fun of prog
  | Trace
and prog = com list
type output = string list
type var_env = (string * const) list

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
  let rec aux acc if_count = function
    | 'E' :: 'n' :: 'd' :: ';' :: rest -> 
      if (if_count = 0) then
        (List.rev acc, rest)
      else 
        aux (';' :: 'd' :: 'n' :: 'E' :: acc) (if_count - 1) rest
    | 'I' :: 'f' :: rest -> aux ('f' :: 'I' :: acc) (if_count + 1) rest
    | c :: rest -> aux (c :: acc) if_count rest
    | [] -> failwith "Unexpected end of input while parsing Fun"
in aux [] 0 xs
and extract_until_else xs =
  let rec aux acc if_count = function
  | 'E' :: 'l' :: 's' :: 'e' :: rest -> 
    if (if_count = 0) then
      (List.rev acc, rest)
    else 
      aux ('e' :: 's' :: 'l' :: 'E' :: acc) (if_count - 1) rest
  | 'I' :: 'f' :: rest -> aux ('f' :: 'I' :: acc) (if_count + 1) rest
  | c :: rest -> aux (c :: acc) if_count rest
  | [] -> failwith "Unexpected end of input while parsing If with Else"
in aux [] 0 xs


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


let push_com (c_stack)(c)=
  (c :: c_stack)

let dup_com (c_stack)=
  match c_stack with
  | x :: xs -> (x :: c_stack)
  | [] -> failwith "Dup failure. Empty stack. Nothing to Duplicate"

let pop_com (c_stack)=
  match c_stack with
  | x :: xs -> xs (*Returns the stack w/o first element*)
  | [] -> failwith "Pop failure. Empty stack. Nothing to Pop"

let not_com (c_stack)=
  match c_stack with
  | (Bool x) :: xs -> push_com xs (Bool(not x))
  | _ :: xs ->  failwith "Not failuire. Top value not boolean"
  | [] ->       failwith "Not failure. Empty stack"

let bind_com (c_stack)(c_var_env)=
    match c_stack with
    | (Symbol x) :: v :: xs -> (xs, ((x, v) :: c_var_env))
    | x :: v :: xs -> failwith "Bind failure. Top of stack is not symbol"
    | x :: xs -> failwith "Bind failure. Only one element in stack"
    | [] -> failwith "Bind failure. Empty stack"

let rec str_of_nat (n : int) : string =
  let d = n mod 10 in 
  let n0 = n / 10 in
  let s = String.make 1 (Char.chr (d + Char.code '0')) in 
  if 0 < n0 then
    (str_of_nat n0) ^ s
  else s
 
let str_of_int (n : int) : string = 
  if n < 0 then
    "-" ^ (str_of_nat (-n))
  else str_of_nat n
 
let toString (c : const) : string =
  match c with
  | Int i -> str_of_int i
  | Bool true -> "True"
  | Bool false -> "False"
  | Unit -> "Unit"
  | Symbol s -> s
  | Closure (s, v, p) -> ("Fun<") ^ (s) ^ (">")
  | _ -> failwith "Trace failure. Invalid constant"

let trace_com (c_stack)(c_out)=
  match c_stack with
  | x :: xs -> 
    let s_res = toString x in
    (xs, (s_res :: c_out))
  | [] -> failwith "Trace failure. Empty stack"


let fun_com (c_stack)(c_var_env)(c1)=
  match c_stack with
  | (Symbol x) :: xs -> push_com xs (Closure(x, c_var_env, c1)) (* Replace the symbol with a function closure named after the symbol, and clone variable environment into it *)
  | x :: xs -> failwith "Function failure. Top value of stack is not symbol"
  | [] -> failwith "Function failure. Empty stack"

(*  Ret command consumes closure, then executes the commands C with variable environment ve   *)
let ret_com (c_stack)=
  match c_stack with
  | Closure (f, vf, c) :: a :: xs -> ((a :: xs), c, vf) (* Return our new stack a :: xs, and the commands we now need to execute*)
  | [] -> failwith "Ret failure. Empty stack"
  | _ -> failwith "Ret failure. Closure and some constant a, do not exist at top of the stack"

let lookup_com (c_stack)(c_var_env)=
  match c_stack with
  | (Symbol x1) :: xs ->
    let rec vloop c_var_env =
      (match c_var_env with
      | (x2, y) :: xy -> 
        if x1 = x2 then
          push_com xs y (*Remove symbol (by ignoring x1) then push value*)
        else
          vloop xy
      | [] -> failwith "Lookup failure. No variable found in variable environment which matches lookup parameter"
      )
    in vloop c_var_env
  | x :: xs -> failwith "Lookup failure. Top of stack is not symbol"
  | [] -> failwith "Lookup failure. Empty stack"

let bopr_com (c_stack)(opr)=
  match opr with
  | Add -> 
    (match c_stack with
    | (Int x1) :: (Int x2) :: xs -> (Int (x1 + x2) :: xs)
    | _ -> failwith "Add failure. Two integers do not exist at the top of the stack"
    )
  | Sub ->
    (match c_stack with
    | (Int x1) :: (Int x2) :: xs -> (Int (x1 - x2) :: xs)
    | _ -> failwith "Subtraction failure. Two integers do not exist at the top of the stack"
    )
  | Mul ->
    (match c_stack with
    | (Int x1) :: (Int x2) :: xs -> (Int (x1*x2) :: xs)
    | _ -> failwith "Multiplication failure. Two integers do not exist at the top of the stack"
    )
  | Div ->
    (match c_stack with
    | (Int x1) :: (Int x2) :: xs -> (Int (x1/x2) :: xs)
    | _ -> failwith "Division failure. Two integers do not exist at the top of the stack"
    )
  | Lt -> 
    (match c_stack with
    | (Int x1) :: (Int x2) :: xs -> (Bool (x1 < x2) :: xs)
    | _ -> failwith "Less than failure. Two integers do not exist at the top of the stack"
    )
  | Gt ->
    (match c_stack with
    | (Int x1) :: (Int x2) :: xs -> (Bool (x1 > x2) :: xs)
    | _ -> failwith "Greater than failure. Two integers do not exist at the top of the stack"
    )
  | Eq -> 
    (match c_stack with
    | (Int x1) :: (Int x2) :: xs -> (Bool (x1 = x2) :: xs)
    | _ -> failwith "Equal-to failure. Two integers do not exist at the top of the stack"
    )
  | And ->
    (match c_stack with
    | (Bool x1) :: (Bool x2) :: xs -> (Bool (x1 && x2) :: xs)
    | _ -> failwith "And failure. Two booleans do not exist at the top of the stack"
    )
  | Or ->
    (match c_stack with
    | (Bool x1) :: (Bool x2) :: xs -> (Bool (x1 || x2) :: xs)
    | _ -> failwith "Or failure. Two booleans do not exist at the top of the stack"
    )
  | Swap ->
    (match c_stack with
    | x1 :: x2 :: xs -> (x2 :: x1 :: xs)
    | _ -> failwith "Swap failure. Two constants do not exist at the top of the stack"
    )
  | Over ->
    (match c_stack with
    | x1 :: x2 :: xs -> push_com (c_stack)(x2) (*Duplicate second item from top of stack and put it on top*)
    | _ -> failwith "Over failure. Two constants do not exist at the top of the stack, such that the second item from the top can be cloned"
    )


(*  Runs whole stack program and returns end result   *)
let execute (s : string) : output=
  let program = parser s in
  let rec helper program c_stack c_var_env c_out = (* approach adheres to the principles of immutability, making the function more modular*)
    match program with
    | [] -> c_out
    | Push c :: rest ->   helper rest (c :: c_stack)            c_var_env c_out
    | Dup :: rest ->      helper rest (dup_com(c_stack))        c_var_env c_out
    | Pop :: rest ->      helper rest (pop_com(c_stack))        c_var_env c_out
    | Add :: rest ->      helper rest (bopr_com(c_stack)(Add))  c_var_env c_out
    | Sub :: rest ->      helper rest (bopr_com(c_stack)(Sub))  c_var_env c_out
    | Mul :: rest ->      helper rest (bopr_com(c_stack)(Mul))  c_var_env c_out
    | Div :: rest ->      helper rest (bopr_com(c_stack)(Div))  c_var_env c_out
    | Swap :: rest ->     helper rest (bopr_com(c_stack)(Swap)) c_var_env c_out
    | Over :: rest ->     helper rest (bopr_com(c_stack)(Over)) c_var_env c_out
    | And :: rest ->      helper rest (bopr_com(c_stack)(And))  c_var_env c_out
    | Or  :: rest ->      helper rest (bopr_com(c_stack)(Or))   c_var_env c_out
    | Not :: rest ->      helper rest (not_com(c_stack))        c_var_env c_out
    | Lt :: rest ->       helper rest (bopr_com(c_stack)(Lt))   c_var_env c_out
    | Gt :: rest ->       helper rest (bopr_com(c_stack)(Gt))   c_var_env c_out
    | Eq :: rest ->       helper rest (bopr_com(c_stack)(Eq))   c_var_env c_out
    | If(c1, c2) :: rest->
      (
      match c_stack with
      | (Bool x) :: xs -> 
          if x = true then 
            helper (c1 @ rest) xs c_var_env c_out
          else 
            helper (c2 @ rest) xs c_var_env c_out
      | x :: xs -> failwith "If Statement failure. No Boolean on top of stack to read"
      | [] -> failwith "If Statement failure. Empty stack"
      )
    (*| Jmp :: rest*)
    (*| Jz :: rest*)
    | Call :: rest ->
      (
      match c_stack with
      | Closure (n, vfe, c) :: a :: xs -> 
        let new_ve = ((n, Closure (n, vfe, c)) :: vfe) in
        let cc_closure = Closure ("cc", c_var_env, rest) in
        helper c (a :: cc_closure :: xs) new_ve c_out
      | _ :: xs -> failwith "Call failure."
      | [] -> failwith "Call failure. Empty stack"  
      )
    | Ret :: rest -> 
      (
      match ret_com(c_stack) with
      | (cs, cc, cve) -> helper (cc @ rest) cs cve c_out (* Append commands from function to our running program, and return our new stack with the closure popped off*)
      | _ -> failwith "Ret did not return stack and new commands"
      )
    | Bind :: rest ->
      (
      match bind_com(c_stack)(c_var_env) with
      | (cs, cve) -> helper rest cs cve c_out (*Extract the current stack and current variable environment from bind's output and pass it into helper*)
      | _ -> failwith "Bind did not return proper environments"
      )
    | Lookup :: rest ->       helper rest (lookup_com(c_stack)(c_var_env))     c_var_env c_out
    | Fun(c1) :: rest ->      helper rest (fun_com(c_stack)(c_var_env)(c1))    c_var_env c_out
    | Trace :: rest ->
      (
      match trace_com(c_stack)(c_out) with
      | (cs, co) -> helper rest cs c_var_env co
      | _ -> failwith "Trace did not return proper stack & output"
      )
  in helper program ([] : const list) [] []