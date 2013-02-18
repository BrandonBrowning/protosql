
module Test

open System
open Common
open FParsec
open Grammar
open Parse

exception TestException of string

let fail s =
    eprintn s
    TestException s |> raise

let testParse str ast =
    match run protoSqlParser str with
        | Success(result, _, _) ->
            if result = ast then () else fail "ASTs differ"
        | Failure(msg, _, _) ->
            "Parsing failed:" + Environment.NewLine + msg |> fail
    ()

let testSimplestParse() =
    // BUG: Forcing me to have an element of each part.
    //      Attempted to fix with "attempts", didn't work.
    let protoSql = "dbo.Foo?5/x{y}"
    let expectedAST = (("", "dbo", "Foo"), [WhereID(WhereIDSimple(PrimativeInt 5))], [(Ascending,("", "", "x"))], [SelectColumn(("", "", "y"))])
    testParse protoSql expectedAST

let test() =
    testSimplestParse()
    printn "Testing completed"