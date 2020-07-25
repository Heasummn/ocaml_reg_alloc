(* Eventually, go from AST -> Expr list (DFS order) -> live intervals *)
type id = string

type constant =
  | Int of int

type bin_op =
  | Plus
  | Minus
  | Mult
  | Div
type value =
  | Constant of constant
  | Var of id
type expr =
  | Value of value
  | BinaryOp of bin_op * value * value
  | Defintion of id * expr
