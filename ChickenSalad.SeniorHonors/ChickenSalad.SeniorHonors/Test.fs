
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
    let protoSql = "dbo.[The quick brown fox jumped!]"
    let expectedAST = (FromTable("", "dbo", "The quick brown fox jumped!"), [], [], [])
    testParse protoSql expectedAST

let testWhereIdOverExpressionIfInt() =
    let protoSql = "five?5"
    let expectedAST = (FromTable("", "", "five"), [ValueExprPrimative(PrimativeInt 5)], [], [])
    testParse protoSql expectedAST

let testWhereExpressionSimpleFunctionCall() =
    let protoSql = "[Some Schema].[redundant]?func(Column, '2013-05-05')"
    let astFuncArgs = [ValueExprPrimative (PrimativeLiteral "Column"); ValueExprPrimative(PrimativeString "2013-05-05")]
    let expectedAST = (FromTable("", "Some Schema", "redundant"), [ValueExprFunctionCall("func", astFuncArgs)], [], [])
    testParse protoSql expectedAST

let testAscendingAndOrderByClauses() =
    let protoSql = @"foo//bar\\baz"
    let expectedAST = (FromTable("", "", "foo"), [], [(Ascending, ("", "", "bar")); (Descending, ("", "", "baz"))], [])
    testParse protoSql expectedAST

let testSpacedOutCode() =
    let protoSql = "dbo.[i like  .spacing. $] ?    42 //foo  \n\\\\rawr { x = 42;\n y = '  test ' }"
    let expectedAST = (FromTable("", "dbo", "i like  .spacing. $"),
        [ValueExprPrimative(PrimativeInt  42)],
        [(Ascending, ("", "", "foo")); (Descending, ("", "", "rawr"))],
        [SelectExpr("x", ValueExprPrimative(PrimativeInt 42)); SelectExpr("y", ValueExprPrimative(PrimativeString "  test "))]
    )
    testParse protoSql expectedAST

let testMultitudeOfOperators() =
    let protoSql = "Person ?x > 5 ?y-3 = 2"

    let firstWhereAST = 
        ValueExprBinaryOperator(">",
            ValueExprPrimative(PrimativeLiteral "x"),
            ValueExprPrimative(PrimativeInt 5))

    let secondWhereAST =
        ValueExprBinaryOperator("=",
            ValueExprBinaryOperator("-",
                ValueExprPrimative(PrimativeLiteral "y"),
                ValueExprPrimative(PrimativeInt 3)),
            ValueExprPrimative(PrimativeInt 2))

    let expectedAST = (FromTable("", "", "Person"), [firstWhereAST; secondWhereAST], [], [])
    testParse protoSql expectedAST

let testParenthesisExpression() =
    let protoSql = "foo?(x / 5) > bar(y)"

    let divisionAST = ValueExprBinaryOperator("/", ValueExprPrimative(PrimativeLiteral "x"), ValueExprPrimative(PrimativeInt 5))
    let barCallAST = ValueExprFunctionCall("bar", [ValueExprPrimative(PrimativeLiteral "y")])
    let expectedAST = (FromTable("", "", "foo"), [ValueExprBinaryOperator(">", divisionAST, barCallAST)], [], [])

    testParse protoSql expectedAST

let testLikeOperator() =
    let protoSql = "foo?x~'bar'"
    let expectedAST = (FromTable("", "", "foo"), [ValueExprBinaryOperator("~", ValueExprPrimative(PrimativeLiteral "x"), ValueExprPrimative(PrimativeString "bar"))], [], [])
    testParse protoSql expectedAST

let testInnerJoin() =
    let protoSql = "dbo.Test --> dbo.Test2()"
    let expectedAST = (FromJoins[(("", "dbo", "Test"), JoinType.InnerJoin, ("", "dbo", "Test2"), ("", ""))], [], [], [])
    testParse protoSql expectedAST

let testOuterJoin() =
    let protoSql = "dbo.Test -=> dbo.Test2()"
    let expectedAST = (FromJoins[(("", "dbo", "Test"), JoinType.OuterJoin, ("", "dbo", "Test2"), ("", ""))], [], [], [])
    testParse protoSql expectedAST

let testCrossJoin() =
    let protoSql = "dbo.Test -x> dbo.Test2()"
    let expectedAST = (FromJoins[(("", "dbo", "Test"), JoinType.CrossJoin, ("", "dbo", "Test2"), ("", ""))], [], [], [])
    testParse protoSql expectedAST

let testMultiColumns() =
    let protoSql = "[database].[schema].[table]?[schema].[table].[column] = 42"
    let expectedAST = (FromTable("database", "schema", "table"), [ValueExprBinaryOperator("=", ValueExprPrimative(PrimativeLiteral "[schema].[table].[column]"), ValueExprPrimative(PrimativeInt 42))], [], [])
    testParse protoSql expectedAST

let testSimpleBitOfEverything() =
    let protoSql = "dbo.Foo?5//x{y}"
    let expectedAST = (FromTable("", "dbo", "Foo"), [ValueExprPrimative(PrimativeInt 5)], [(Ascending,("", "", "x"))], [SelectColumn(("", "", "y"))])
    testParse protoSql expectedAST

let test() =
    let tests = [
        testTwoPartTableSelectStar;
        testWhereIdOverExpressionIfInt;
        testWhereExpressionSimpleFunctionCall;
        testAscendingAndOrderByClauses;
        testSpacedOutCode;
        testMultitudeOfOperators;
        testParenthesisExpression;
        testLikeOperator;
        testInnerJoin; testOuterJoin; testCrossJoin;
        // ignored testMultiColumns;
        testSimpleBitOfEverything
    ]

    let mutable all_correct = true
    for test in tests do
        let success = test()
        if not success then
            all_correct <- false
    
    if all_correct then printn "All tests passed!"