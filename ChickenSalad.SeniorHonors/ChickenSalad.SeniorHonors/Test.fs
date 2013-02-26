
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
            if result = ast then
                true
            else
                printn "Error: ASTs differ"
                printn ""

                printn "query"
                printn str
                printn ""

                printn "expected"
                printfn "%A" ast
                printn ""

                printn "got"
                printfn "%A" result

                false
        | Failure(msg, _, _) ->
            "Parsing failed:" + Environment.NewLine + msg |> printn
            false

let testTwoPartTableSelectStar() =
    let protoSql = "dbo.[Rawr omg!]"
    let expectedAST = (("", "dbo", "Rawr omg!"), [], [], [])
    testParse protoSql expectedAST

let testWhereIdOverExpressionIfInt() =
    let protoSql = "dbo.[i like turtles]?5"
    let expectedAST = (("", "dbo", "i like turtles"), [WhereID(WhereIDSimple(PrimativeInt 5))], [], [])
    testParse protoSql expectedAST

let testWhereExpressionSimpleFunctionCall() =
    let protoSql = "[Some Schema].[redundant]?func(Column, '2013-05-05')"
    let astFuncArgs = [ValueExprPrimative (PrimativeLiteral "Column"); ValueExprPrimative (PrimativeString "2013-05-05")]
    let expectedAST = (("", "Some Schema", "redundant"), [WhereExpr(ValueExprFCall("func", astFuncArgs))], [], [])
    testParse protoSql expectedAST

let testAscendingAndOrderByClauses() =
    let protoSql = @"foo/bar\baz"
    let expectedAST = (("", "", "foo"), [], [(Ascending, ("", "", "bar")); (Descending, ("", "", "baz"))], [])
    testParse protoSql expectedAST

let testSpacedOutCode() =
    let protoSql = "dbo.[i like  .spacing. $] ?    42 /foo  \n\\rawr { x = 42;\n y = '  test ' }"
    let expectedAST = (("", "dbo", "i like  .spacing. $"),
        [WhereID(WhereIDSimple(PrimativeInt 42))],
        [(Ascending, ("", "", "foo")); (Descending, ("", "", "rawr"))],
        [SelectExpr("x", ValueExprPrimative(PrimativeInt 42)); SelectExpr("y", ValueExprPrimative(PrimativeString "  test "))]
    )
    testParse protoSql expectedAST

let testSimpleBitOfEverything() =
    let protoSql = "dbo.Foo?5/x{y}"
    let expectedAST = (("", "dbo", "Foo"), [WhereID(WhereIDSimple(PrimativeInt 5))], [(Ascending,("", "", "x"))], [SelectColumn(("", "", "y"))])
    testParse protoSql expectedAST

let test() =
    let tests = [
        testTwoPartTableSelectStar;
        testWhereIdOverExpressionIfInt;
        testWhereExpressionSimpleFunctionCall;
        testAscendingAndOrderByClauses;
        testSpacedOutCode;
        testSimpleBitOfEverything
    ]

    let mutable all_correct = true
    for test in tests do
        let success = test()
        if not success then
            all_correct <- false
    
    if all_correct then printn "All tests passed!"