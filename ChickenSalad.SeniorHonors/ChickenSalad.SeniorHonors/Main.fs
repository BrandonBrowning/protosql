
module Main
open System
open FParsec
open Parsing
open ProtoSql
open ProtoSqlParser

let translatePrimative = function
    | PrimativeInt(i) -> i.ToString()
    | PrimativeFloat(i) -> i.ToString()
    | PrimativeString(s) -> sprintf "'%s'" s
    | PrimativeLiteral(s) -> s

let rec translateExpr = function
    | ValueExprPrimative(prim) -> translatePrimative prim
    | ValueExprFCall(ident, args) -> 
        let argList = String.Join(", ", List.map translateExpr args)
        sprintf "%s(%s)" ident argList

let translateTableOrColumn (a, b, c) =
    [a; b; c] 
        |> Seq.filter (not << String.IsNullOrEmpty)
        |> fun cs -> String.Join(".", cs)

let translateFrom (a, b, c) = "FROM " + translateTableOrColumn (a, b, c)

let translateWhere where = 
    "WHERE " + (
        match where with
            | WhereID(whereID) -> 
                match whereID with
                    | WhereIDSimple(prim) -> translatePrimative prim
                    | WhereIDComposite(exprs) -> 
                        let pieceList = String.Join(", ", List.map translateExpr exprs)
                        sprintf "(%s)" pieceList
            | WhereValueExpr(expr) -> translateExpr expr
    )

let translateSelect select =
    let translateSelectLine = function
        | SelectLineColumn(col) -> translateTableOrColumn col
        | SelectLineExpr(ident, expr) -> translateExpr expr + " AS " + ident
    
    "SELECT " + String.Join(", ", List.map translateSelectLine select)

let translateOrderBy orderby =
    let translateOrderByColumnType = function 
        | Ascending -> "ASCENDING"
        | Descending -> "DESCENDING"

    let translateOrderByColumn (typ, col) =
        translateTableOrColumn col + " " + translateOrderByColumnType typ

    let convertedClauses = List.map translateOrderByColumn orderby

    "ORDER BY " + String.Join(", ", convertedClauses)

let extract f def option = match option with
    | Some x -> f x
    | None -> def

let extractString f = extract f ""

let translate = function
    | Query(f, wo, oo, so) -> 
        [(extractString translateSelect so);
        (translateFrom f);
        (extractString translateWhere wo);
        (extractString translateOrderBy oo)]
            |> Seq.filter (not << String.IsNullOrEmpty)
            |> fun parts -> String.Join(Environment.NewLine, parts)

[<EntryPoint>]
let main args =
    let query = "customer.AccountPolicyCoverage?has(StartDate)\\EndDate{duration=between(day, StartDate, EndDate)}"
    let result = run protoSqlParser query

    printn query
    printn "---"

    match result with
        | Success(result, _, _)   ->
            printfn "Success: %A" result
            printn "---"
            printn <| translate result
        | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

    Console.ReadLine() |> ignore
    0