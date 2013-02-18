
module Cross

open System
open Grammar

let crossPrimative = function
    | PrimativeInt(i) -> i.ToString()
    | PrimativeFloat(i) -> i.ToString()
    | PrimativeString(s) -> sprintf "'%s'" s
    | PrimativeLiteral(s) -> s

let isOperator = function
    | ValueExprBinOp(_, _, _) -> true
    | _ -> false

let rec crossExpr = function
    | ValueExprPrimative(prim) -> crossPrimative prim
    | ValueExprBinOp(ident, left, right) ->
        let leftExpr = crossExpr left
        let leftDisplay = (if isOperator left then sprintf "(%s)" leftExpr else leftExpr)

        let rightExpr = crossExpr right
        let rightDisplay = (if isOperator right then sprintf "(%s)" rightExpr else rightExpr)

        sprintf "%s %s %s" leftDisplay ident rightDisplay
    | ValueExprFCall(ident, args) ->
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
            | WhereID(whereID) -> 
                match whereID with
                    | WhereIDSimple(prim) -> "*ID* = " + crossPrimative prim
                    | WhereIDComposite(exprs) -> 
                        let pieceList = String.Join(", ", List.map crossExpr exprs)
                        sprintf "*ID* = (%s)" pieceList
            | WhereValueExpr(expr) -> crossExpr expr

    "WHERE " + String.Join(" AND ", List.map crossWhereLine wheres)

let crossSelect select =
    let crossSelectLine = function
        | SelectLineColumn(col) -> crossTableOrColumn col
        | SelectLineExpr(ident, expr) -> crossExpr expr + " AS " + ident
    
    "SELECT " + String.Join(", ", List.map crossSelectLine select)

let crossOrderBy orderby =
    let crossOrderByColumnType = function 
        | Ascending -> "ASCENDING"
        | Descending -> "DESCENDING"

    let crossOrderByColumn (typ, col) =
        crossTableOrColumn col + " " + crossOrderByColumnType typ

    let convertedClauses = List.map crossOrderByColumn orderby

    "ORDER BY " + String.Join(", ", convertedClauses)

let extract f def option = match option with
    | Some x -> f x
    | None -> def

let extractString f = extract f ""

let cross = function
    | Query(f, wo, oo, so) -> 
        [(extract crossSelect "SELECT *" so);
        (crossFrom f);
        (extractString crossWhere wo);
        (extractString crossOrderBy oo)]
            |> Seq.filter (not << String.IsNullOrEmpty)
            |> fun parts -> String.Join(Environment.NewLine, parts)