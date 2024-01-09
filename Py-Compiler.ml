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
| Neg | Not | Abs | IntConvert
type bopr =
| Add | Sub | Mul | Div | Mod | Rem
| And | Or
| Lt | Gt | Lte | Gte | Eq
| Pow | Floor
(*| Is | IsNot*)
type expr =
| Int of int | Bool of bool | Float of float | Char of char | String of string | Unit
| UOpr of uopr * expr
| BOpr of bopr * expr * expr
| Var of string
| Param of string * expr
| Seq of expr * expr
| Ifte of expr * expr * expr
| While of expr * expr
| For of expr * expr
| Def of string * expr
(*| Lambda of *)
| Break
| Continue
| Trace of expr


let rec py_parse_int(xs : char list): expr option = 
  let rec helper xs accum
    match xs with
    | x :: xs when x = '-' -> -1 * helper xs 0
    | x :: xs when x >= '0' && x <= '9' -> helper xs (10*accum + (digit_of_char x))
    | [] -> None
    | _ -> None
  in helper xs 0

let rec py_parse_matcher(xs : char list)=
  let ws = whitespace xs in
  let rec helper xs accum =
  
  in List.rev(helper ws ([] : expr))

let rec py_parse(s : string) : expr =
  py_parse_matcher(string_listize(s))