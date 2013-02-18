
module Grammar

open System
open Common
open FParsec

type ProtoSql = From * Where list * OrderBy list * Select list

and From = Dataset
and Dataset = Table // TODO: Make this allow dataset-valued expressions

and Where = 
    | WhereID of WhereID
    | WhereExpr of ValueExpr

and WhereID = 
    | WhereIDSimple of Primative
    | WhereIDComposite of ValueExpr list

and OrderBy = OrderByType * Column
and OrderByType = Ascending | Descending

and Select =
    | SelectColumn of Column
    | SelectExpr of string * ValueExpr

and ValueExpr =
    | ValueExprPrimative of Primative
    | ValueExprBinOp of BinOp
    | ValueExprFCall of FCall

and BinOp = string * ValueExpr * ValueExpr
and FCall = string * ValueExpr list

and Table = string * string * string
and Column = string * string * string

and Primative =
    | PrimativeInt of int
    | PrimativeFloat of float
    | PrimativeString of string
    | PrimativeLiteral of string