
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

let isOperator = function
    | ValueExprBinOp(_, _, _) -> true
    | _ -> false

let rec translateExpr = function
    | ValueExprPrimative(prim) -> translatePrimative prim
    | ValueExprBinOp(ident, left, right) ->
        let leftExpr = translateExpr left
        let leftDisplay = (if isOperator left then sprintf "(%s)" leftExpr else leftExpr)

        let rightExpr = translateExpr right
        let rightDisplay = (if isOperator right then sprintf "(%s)" rightExpr else rightExpr)

        sprintf "%s %s %s" leftDisplay ident rightDisplay
    | ValueExprFCall(ident, args) ->
        let argList = String.Join(", ", List.map translateExpr args)
        sprintf "%s(%s)" ident argList

let translateTableOrColumn (a, b, c) =
    [a; b; c] 
        |> Seq.filter (not << String.IsNullOrEmpty)
        |> fun cs -> String.Join(".", cs)

let translateFrom (a, b, c) = "FROM " + translateTableOrColumn (a, b, c)

// TODO: Add connecting to database and finding ID
let translateWhere wheres = 
    let translateWhereLine where = 
        match where with
            | WhereID(whereID) -> 
                match whereID with
                    | WhereIDSimple(prim) -> "*ID* = " + translatePrimative prim
                    | WhereIDComposite(exprs) -> 
                        let pieceList = String.Join(", ", List.map translateExpr exprs)
                        sprintf "*ID* = (%s)" pieceList
            | WhereValueExpr(expr) -> translateExpr expr

    "WHERE " + String.Join(" AND ", List.map translateWhereLine wheres)

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
        [(extract translateSelect "SELECT *" so);
        (translateFrom f);
        (extractString translateWhere wo);
        (extractString translateOrderBy oo)]
            |> Seq.filter (not << String.IsNullOrEmpty)
            |> fun parts -> String.Join(Environment.NewLine, parts)

let opp = new OperatorPrecedenceParser<_,_,_>()

let adjustPosition offset (pos: Position) =
    Position(pos.StreamName, pos.Index + int64 offset,
             pos.Line, pos.Column + int64 offset)

// To simplify infix operator definitions, we define a helper function.
let addInfixOperator str prec assoc mapping =
    let op = InfixOperator(str, getPosition .>> spaces, prec, assoc, (),
                           fun opPos leftTerm rightTerm ->
                               mapping
                                   (adjustPosition -str.Length opPos)
                                   leftTerm rightTerm)
    opp.AddOperator(op)

// Of course, you can define similar functions for other operator types.

// With the helper function in place, you can define an operator with
// a mapping function that gets passed the text location of the
// parsed operator as the first argument.
addInfixOperator "+" 1 Associativity.Left (fun opPos leftTerm rightTerm -> ValueExprBinOp("+", leftTerm, rightTerm))
addInfixOperator "-" 1 Associativity.Left (fun opPos leftTerm rightTerm -> ValueExprBinOp("-", leftTerm, rightTerm))
addInfixOperator "*" 2 Associativity.Left (fun opPos leftTerm rightTerm -> ValueExprBinOp("*", leftTerm, rightTerm))
addInfixOperator "/" 2 Associativity.Left (fun opPos leftTerm rightTerm -> ValueExprBinOp("/", leftTerm, rightTerm))

opp.TermParser <- parseValueExpr

let protoSqlWithOps = opp.ExpressionParser

[<EntryPoint>]
let main args =
    let query = "table.name?gt(StartDate,getdate())/StartDate{foo=bar;baz=StartDate+1}" // @"customer.Account?gt(StartDate,'2012-05-16'){EmploymentLength=datediff(getdate(), StartDate); Foo=bar+1}"
    let parser = parseValueExpr

    let result = run protoSqlParser query

    printn query
    printn "---"

    match result with
        | Success(result, _, _) ->
            printfn "Success: %A" result
            printn "---"
            printn <| translate result
        | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

    Console.ReadLine() |> ignore
    0