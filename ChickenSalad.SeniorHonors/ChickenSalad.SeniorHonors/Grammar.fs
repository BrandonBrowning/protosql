
module Grammar

open System
open Common
open FParsec

type ProtoSql = From * Where list * OrderBy list * Select list

and From = Dataset
and Dataset = Table // TODO: Make this allow dataset-valued expressions

and Where = 
    | WhereCompoundKey of ValueExpr list
    | WhereExpr of ValueExpr

and OrderBy = OrderByType * Column
and OrderByType = Ascending | Descending

and Select =
    | SelectColumn of Column
    | SelectExpr of string * ValueExpr

and ValueExpr =
    | ValueExprPrimative of Primative
    | ValueExprBinaryOperator of BinaryOperator
    | ValueExprFunctionCall of FunctionCall

and BinaryOperator = string * ValueExpr * ValueExpr
and FunctionCall = string * ValueExpr list

and Table = string * string * string
and Column = string * string * string

and Primative =
    | PrimativeInt of int
    | PrimativeFloat of float
    | PrimativeString of string
    | PrimativeBoolean of bool
    | PrimativeLiteral of string