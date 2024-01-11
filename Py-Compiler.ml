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
| UOpr of uopr * expr
| BOpr of bopr * expr * expr
| Var of string
| Param of string * expr
| Seq of expr list
| Ifte of expr * expr * expr
| While of expr * expr
| For of expr * expr * expr
| Def of string * expr * expr
(*| Lambda of *)
| Break
| Continue
| Trace of expr

(*Modified version of interpreter's whitespace function. \t is not considered a whitespace in python. We need to read indents for parsing*)
let rec py_whitespace (xs : char list) : char list =
 let rec helper xs accum =
  match xs with
  | ' ' :: rest -> helper rest accum
  | '\n' :: rest -> helper rest accum
  | ',' :: rest -> helper rest accum
  | c :: rest -> helper rest (c :: accum)
  | _ -> accum
in List.rev(helper xs ([] : char list))

let rec py_parse_expr(xs : char list) =
  let ws = py_whitespace xs in
  let rec helper xs accum =
    match xs with
    | 'T' :: 'r' :: 'u' :: 'e' ::  rest ->                            helper rest (Bool(true))
    | 'F' :: 'a' :: 'l' :: 's' :: 'e' ::  rest ->                     helper rest (Bool(false))

    | 'b' :: 'r' :: 'e' :: 'a' :: 'k' :: rest ->                      helper rest (Break :: accum)
    | 'c' :: 'o' :: 'n' :: 't' :: 'i' :: 'n' :: 'u' :: 'e' :: rest -> helper rest (Continue :: accum)
    | 'w' :: 'h' :: 'i' :: 'l' :: 'e' :: rest -> 
      let condition, rest_after_cond = extract_until_colon rest in
      let body, rest_after_body =      extract_until_indent rest_after_cond in
      helper rest_after_body (While (parse_expr condition, parse_expr body) :: accum)
    | 'i' :: 'f' :: rest ->
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
      
  in List.rev(helper ws ([] : expr list))

let rec py_parse(s : string) : expr list =
  py_parse_expr(string_listize(s))