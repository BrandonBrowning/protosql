
module ProtoSql
open System
open FParsec
open Parsing

type ProtoSql = 
    Query of Dataset * Where option * OrderBy option * Select option

and Dataset = Table // TODO: Make this allow dataset-valued expressions

and Where = WhereID of WhereID | WhereValueExpr of ValueExpr
and WhereID = Primative | Tuple
and Tuple = ValueExpr list

and OrderBy = OrderByColumn list
and OrderByColumn = OrderByColumnType * Column
and OrderByColumnType = Ascending | Descending

and Select = SelectLine list
and SelectLine = SelectLineColumn of Column | SelectLineExpr of string * ValueExpr

and ValueExpr = ValueExprPrimative of Primative | ValueExprFCall of FCall
and FCall = FCall of string * ValueExpr list

and Table = Table of string * string * string
and Column = Column of string * string * string

and Primative =
    | PrimativeInt of int
    | PrimativeFloat of float
    | PrimativeString of string
    | PrimativeLiteral of string