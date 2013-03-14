
module Cross

open System
open Grammar

let crossPrimative = function
    | PrimativeInt(i) -> i.ToString()
    | PrimativeFloat(i) -> i.ToString()
    | PrimativeString(s) -> sprintf "'%s'" s
    | PrimativeLiteral(s) -> s

let isOperator = function
    | ValueExprBinaryOperator(_, _, _) -> true
    | _ -> false

let translateBinopName = function
    | "||" -> "OR"
    | "&&" -> "AND"
    | x    -> x

let rec crossExpr = function
    | ValueExprPrimative(prim) -> crossPrimative prim
    | ValueExprBinaryOperator(ident, left, right) ->
        let leftExpr = crossExpr left
        let leftDisplay = (if isOperator left then sprintf "(%s)" leftExpr else leftExpr)

        let rightExpr = crossExpr right
        let rightDisplay = (if isOperator right then sprintf "(%s)" rightExpr else rightExpr)

        sprintf "%s %s %s" leftDisplay (translateBinopName ident) rightDisplay
    | ValueExprFunctionCall(ident, args) ->
        let argList = String.Join(", ", List.map crossExpr args)
        sprintf "%s(%s)" ident argList

let crossTableOrColumn (a, b, c) =
    [a; b; c] 
        |> Seq.filter (not << String.IsNullOrEmpty)
        |> fun cs -> String.Join(".", cs)

let crossFrom (a, b, c) = "FROM " + crossTableOrColumn (a, b, c)

// TODO: Add connecting to database and finding ID
let crossWhere wheres = 
    let crossWhereLine where = 
        match where with
            | ValueExprPrimative (PrimativeInt x) -> sprintf "*ID* = %d" x
            | _ -> crossExpr where

    "WHERE " + String.Join(" AND ", List.map crossWhereLine wheres)

let crossSelect select =
    let crossSelectLine = function
        | SelectColumn(col) -> crossTableOrColumn col
        | SelectExpr(ident, expr) -> crossExpr expr + " AS " + ident
    
    "SELECT " + String.Join(", ", List.map crossSelectLine select)

let crossOrderBy orderby =
    let crossOrderByType = function 
        | Ascending -> "ASCENDING"
        | Descending -> "DESCENDING"

    let crossOrderByColumn (typ, col) =
        crossTableOrColumn col + " " + crossOrderByType typ

    let convertedClauses = List.map crossOrderByColumn orderby

    "ORDER BY " + String.Join(", ", convertedClauses)

let empty xs = List.length xs = 0

let cross (from, where, orderBy, select): string =
    [
        (if empty select then "SELECT *" else crossSelect select);
        (crossFrom from);
        (if empty where then "" else crossWhere where);
        (if empty orderBy then "" else crossOrderBy orderBy)
    ]
        |> Seq.filter (not << String.IsNullOrEmpty)
        |> fun parts -> String.Join(Environment.NewLine, parts)