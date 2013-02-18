
module Main

open System
open Common
open Cross
open FParsec
open Grammar
open Optimize
open Parse

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
    let query = "table.name?gt(StartDate,getdate())/StartDate{foo=bar;baz=3+1}" // @"customer.Account?gt(StartDate,'2012-05-16'){EmploymentLength=datediff(getdate(), StartDate); Foo=bar+1}"
    let parser = parseValueExpr

    let result = run protoSqlParser query

    printn query
    printn "---"

    match result with
        | Success(result, _, _) ->
            let sql = cross result
            let optimizedSql = cross <| optimize result

            printfn "Success: %A" result
            printn "---"
            printn sql
            printn "---"
            printn optimizedSql
        
        | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

    Console.ReadLine() |> ignore
    0