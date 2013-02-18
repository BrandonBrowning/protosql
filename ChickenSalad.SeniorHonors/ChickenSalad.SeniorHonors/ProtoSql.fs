
module ProtoSql
open System
open FParsec
open Parsing

type ProtoSql = 
    Query of From * Where list option * OrderBy option * Select option

and From = Dataset
and Dataset = Table // TODO: Make this allow dataset-valued expressions

and Where = 
    | WhereID of WhereID
    | WhereValueExpr of ValueExpr

and WhereID = 
    | WhereIDSimple of Primative
    | WhereIDComposite of ValueExpr list

and OrderBy = OrderByColumn list
and OrderByColumn = OrderByColumnType * Column
and OrderByColumnType = Ascending | Descending

and Select = SelectLine list
and SelectLine = 
    | SelectLineColumn of Column
    | SelectLineExpr of string * ValueExpr

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