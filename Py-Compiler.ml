(*
Interp
Source program → interpreter → output

Source Program → compiler (generates a program that translates from high-level to level language) → Target program → output

Compiler have lot of middle steps to translate

For Our Case
Source prog → Translator → intermediate program → virtual machine (interpreter) → output
Compile: string (input program) -> string (stack language program)
*)
#use "stacklang.ml"
type uopr =
| Not | Abs
type bopr =
| Add | Sub | Mul | Div | Mod
| And | Or
| Lt | Gt | Lte | Gte | Eq
| Pow
(*| Is | IsNot*)
type expr =
| Int of int | Bool of bool | Float of float (*| Char of char | String of string*)
| UOpr of uopr * pro
| BOpr of bopr * pro * pro
| Var of string
| Param of string * pro
| Seq of pro
| Ifte of pro * pro * pro
| While of pro * pro
| For of pro * pro * pro
| Def of string * pro * pro
(*| Lambda of *)
| Break
| Continue
| Print of pro
and pro = expr list

(*Modified version of interpreter's whitespace function. \t is not considered a whitespace in python. We need to read indents for parsing*)
let rec py_whitespace (xs : char list) : char list =
 let rec helper xs accum =
  match xs with
  | ',' :: rest -> helper rest accum
  | c :: rest -> helper rest (c :: accum)
  | _ -> accum
in List.rev(helper xs ([] : char list))

(* x = 50 | 5+5 | *)
let rec py_parse_int accum = function
  | x :: xs when x = '-' -> -1 * py_parse_int 0 xs
  | x :: xs when x >= '0' && x <= '9' -> py_parse_int (10*accum + (digit_of_char x)) xs
  | x :: xs -> accum (*Stop on any non-digit character*)
  | [] -> accum
and strip_int = function
  | x :: xs when x = '-' -> strip_int xs
  | x :: xs when x >= '0' && x <= '9' -> strip_int xs
  | x :: xs -> xs (*Stop on any non-digit character*)
  | [] -> []

(* x = 5, Stop upon equal sign*)
let rec parse_var accum = function
  | x :: xs when (x >= 'A' && x <= 'Z') || (x >= 'a' && x <= 'z') -> parse_var (accum ^ String.make 1 x) xs
  | x :: xs when x = ' ' -> parse_var accum xs
  | x :: xs when x = '=' -> accum
  | [] -> accum
and strip_var = function
  | x :: xs when (x >= 'A' && x <= 'Z') || (x >= 'a' && x <= 'z') -> strip_var xs
  | x :: xs when x = ' ' -> strip_var xs
  | x :: xs when x = '=' -> xs
  | [] -> []


let rec eu_colon xs=
  let rec aux xs acc=
    match xs with
    | ':' :: rest -> (List.rev(acc), rest)
    | x :: rest -> aux rest (x :: acc)
  in aux xs []

(*Edit functions below to make recursive. If a nested parameter or loop is contained in the outside loop, it may stop checking early
   Thus include a parenthasis account or 'in' count*)
(*Extract Until Open Parenthasis is only used to check for the name of a function before its open parenthasis. Does not need to be recursive*)
let rec eu_open_paren xs pCount =
  let rec aux xs acc=
    match xs with
    | '(' :: rest -> (List.rev(acc), rest)
    | x :: rest -> aux rest (x :: acc)
  in aux xs []

let rec eu_close_paren xs=
  let rec aux xs acc pCount=
    match xs with
    | ')' :: rest -> 
      if (pCount = 0) then
        (List.rev(acc), rest)
      else 
                      aux rest (')' :: acc) (pCount - 1)
    | '(' :: rest ->  aux rest ('(' :: acc) (pCount + 1)
    | x :: rest ->    aux rest (x :: acc) pCount
  in aux xs [] 0

let rec eu_in xs =
  let rec aux xs acc=
    match xs with
    | 'i' :: 'n' :: rest -> (List.rev(acc), rest)
    | x :: rest -> aux rest (x :: acc)
  in aux xs []

let rec eu_close_brack xs=
  let rec aux xs acc bCount=
    match xs with
    | ']' :: rest -> 
      if (bCount = 0) then
        (List.rev(acc), rest)
      else 
                      aux rest (']' :: acc) (bCount - 1)
    | '[' :: rest ->  aux rest ('[' :: acc) (bCount + 1)
    | x :: rest ->    aux rest (x :: acc) bCount
  in aux xs [] 0

let rec eu_indent xs=
  let rec aux xs acc iCount=
    (match xs with
    | '\n' :: rest ->
      (
      let nli = iloop rest in
      if nli = 0 then
        (*(List.rev (acc @ ['\n']), rest)*)
        (List.rev(acc), rest)
      else if nli < iCount then
        (*(List.rev (acc @ ['\n']), xs)*)
        (List.rev(acc), rest)
      else
        aux rest acc nli
      )
    | 'i' :: 'f' :: rest ->                      aux rest ('f' :: 'i' :: acc) (iCount + 1)
    | 'f' :: 'o' :: 'r' :: rest ->               aux rest ('r' :: 'o' :: 'f' :: acc) (iCount + 1)
    | 'd' :: 'e' :: 'f' :: rest ->               aux rest ('f' :: 'e' :: 'd' :: acc) (iCount + 1)
    | 'w' :: 'h' :: 'i' :: 'l' :: 'e' :: rest -> aux rest ('e' :: 'l' :: 'i' :: 'h' :: 'w' :: acc) (iCount + 1)
    | x :: rest ->                               aux rest (x :: acc) iCount
    )
    in aux xs [] 1
and iloop xs=
    let rec loop xs count=
      match xs with
      | ' ' :: rest -> loop rest (count + 1)
      | _ :: rest -> count
      | [] -> count
    in loop xs 0


let rec py_parse_expr(xs : char list) =
  let ws = py_whitespace xs in
  let rec helper xs accum =
    match xs with
    | x :: rest when (x >= '0' && x <= '9') || (x = '-') -> (
      let n = py_parse_int 0 (x :: rest) in
      let xss = strip_int rest in
      helper xss (Int (n) :: accum)
     )
    | 'T' :: 'r' :: 'u' :: 'e' ::  rest ->                            helper rest (Bool(true) :: accum)
    | 'F' :: 'a' :: 'l' :: 's' :: 'e' ::  rest ->                     helper rest (Bool(false) :: accum)
    | 'b' :: 'r' :: 'e' :: 'a' :: 'k' :: rest ->                      helper rest (Break :: accum)
    | 'c' :: 'o' :: 'n' :: 't' :: 'i' :: 'n' :: 'u' :: 'e' :: rest -> helper rest (Continue :: accum)
    | 'w' :: 'h' :: 'i' :: 'l' :: 'e' :: rest -> (
      let c, rest_after_cond = eu_colon rest in
      let b, rest_after_body = eu_indent rest_after_cond in
      helper rest_after_body (While(py_parse_expr(c), py_parse_expr(b)) :: accum)
    )
    | 'f' :: 'o' :: 'r' :: rest -> (
      let v, rest_after_in = eu_in rest in
      let s, rest_after_seq = eu_colon rest_after_in in
      let b, rest_after_body = eu_indent rest_after_seq in
      helper rest_after_body (For(py_parse_expr(v), py_parse_expr(s), py_parse_expr(b)) :: accum)
    )
    | x :: rest when (x >= 'A' && x <= 'Z') || (x >= 'a' && x <= 'z') -> (
      let s = parse_var "" (x :: rest) in
      let xss = strip_var rest in
      helper xss (Var (s) :: accum)
    )
    | ' ' :: rest -> helper rest accum
    | [] -> accum
  in List.rev(helper ws ([] : expr list))

let rec py_parse(s : string) : expr list =
  py_parse_expr(string_listize(s))

(*| 'i' :: 'f' :: rest ->
      let condition, rest_after_cond = extract_until_colon rest in
      let b1, rest_after_b1 =          extract_until_indent rest_after_cond in
      let b2, rest_after_b2 =          extract_until_indent rest_after_b1 in
      helper rest_after_b2 (Ifte (parse_expr condition, parse_expr b1, parse_expr b2) :: accum)
    | 'f' :: 'o' :: 'r' :: rest -> 
      let var, rest_after_in =          extract_until_in rest in
      let sequence, rest_after_seq =    extract_until_colon rest_after_in in
      let body, rest_after_body =       extract_until_indent rest_after_seq in
      helper rest_after_body (For (parse_expr var, parse_expr sequence, parse_expr body) :: accum)
    | 'd' :: 'e' :: 'f' :: rest -> 
      (*For now only works with one parameter functions*)
      let name, rest_after_name =       extract_until_parenthasis rest in
      let param, rest_after_param =     extract_until_colon rest_after_name in
      let body, rest_after_body =       extract_until_indent rest_after_param in
      helper rest_after_body (Def (parse_expr name, parse_expr param, parse_expr body) :: accum)
    | 'p' :: 'r' :: 'i' :: 'n' :: 't' :: '(' :: rest ->
      let body, rest_after_body =       extract_until_endparenthasis rest in
      helper rest_after_body (Print (parse_expr body) :: accum)
    | '[' :: rest ->
      let body, rest_after_body =       extract_until_bracket rest in
      helper rest_after_body (Seq (parse_expr body) :: accum)*)