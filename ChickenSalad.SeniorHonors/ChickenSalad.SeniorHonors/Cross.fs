
module Cross

open System
open Common
open Grammar

let tabbing = "    "
let joinLines = sjoin newline

let crossTableOrColumn (a, b, c) =
    [a; b; c] 
        |> Seq.filter (not << String.IsNullOrEmpty)
        |> fun cs -> String.Join(".", cs)

let crossPrimative = function
    | PrimativeInt(i) -> i.ToString()
    | PrimativeFloat(i) -> i.ToString()
    | PrimativeString(s) -> sprintf "'%s'" s
    | PrimativeBoolean(s) -> match s with
        | true -> "1"
        | false -> "0"
    | PrimativeColumn(c) -> crossTableOrColumn c
    | PrimativeLiteral(s) ->
        if s = "null" then "NULL" else s

let isOperator = function
    | ValueExprBinaryOperator(_, _, _) -> true
    | _ -> false

let translateBinopName = function
    | "||" -> "OR"
    | "&&" -> "AND"
    | "~" -> "LIKE"
    | "!~" -> "NOT LIKE"
    | x    -> x

let isNullVExpr = function
    | ValueExprPrimative(PrimativeLiteral("null")) -> true
    | _ -> false

let rec crossExpr = function
    | ValueExprPrimative(prim) -> crossPrimative prim
    | ValueExprBinaryOperator(ident, left, right) ->
        let leftExpr = crossExpr left
        let leftDisplay = (if isOperator left then sprintf "(%s)" leftExpr else leftExpr)

        let rightExpr = crossExpr right
        let rightDisplay = (if isOperator right then sprintf "(%s)" rightExpr else rightExpr)

        if ident = "!=" && (isNullVExpr left || isNullVExpr right) then
            sprintf "%s %s %s" leftDisplay "IS NOT" rightDisplay
        else
            sprintf "%s %s %s" leftDisplay (translateBinopName ident) rightDisplay
    | ValueExprFunctionCall(ident, args) ->
        let argList = String.Join(", ", List.map crossExpr args)
        sprintf "%s(%s)" ident argList

let optionallyBracket (s: string) =
    let needsBrackets = not <| rmatch s "^\w[\w\d]*$"
    if needsBrackets then
        "[" + s + "]"
    else
        s

let crossJoinType = function
    | InnerJoin -> "INNER JOIN"
    | OuterJoin -> "OUTER JOIN"
    | CrossJoin -> "CROSS JOIN"

let lastTableName = function
    | ("", "", name) -> name
    | ("", _, name)  -> name
    | (_, _, name)   -> name

let generateJoinCriteria toTable columns =
    match columns with
        | ("", "")      -> let column = lastTableName toTable + "ID" in (column, column)
        | (fromCol, "") -> (fromCol, fromCol)
        | cols          -> cols

let crossJoin (fromTable, joinType, toTable, columns) =
    match joinType with
        | CrossJoin ->
            sprintf "%s %s" (crossJoinType joinType) (crossTableOrColumn toTable)
        | _         ->
            let (fromColumn, toColumn) = generateJoinCriteria toTable columns
            let fromLocation = crossTableOrColumn fromTable + "." + fromColumn
            let toLocation = crossTableOrColumn toTable + "." + toColumn

            let joinLine = sprintf "%s %s" (crossJoinType joinType) (crossTableOrColumn toTable)
            let onLine = tabbing + sprintf "ON %s = %s" toLocation fromLocation

            joinLine + newline + onLine

let crossFrom = function
    | FromTable(table) -> "FROM " + crossTableOrColumn table
    | FromJoins(joins) ->
        assert (joins.Length > 0)
        let (firstJoinTable, _, _, _) = joins.[0]
        let fromLine = "FROM " + crossTableOrColumn firstJoinTable
        fromLine :: (List.map crossJoin joins) |> joinLines

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
        |> fun parts -> String.Join(newline, parts)