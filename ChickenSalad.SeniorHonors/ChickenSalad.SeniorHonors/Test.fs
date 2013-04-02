
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
    let protoSQL = "dbo.[The quick brown fox jumped!]"
    let expectedAST = (FromTable("", "dbo", "[The quick brown fox jumped!]"), [], [], [])
    testParse protoSQL expectedAST

let testWhereIdOverExpressionIfInt() =
    let protoSQL = "five?5"
    let expectedAST = (FromTable("", "", "five"), [ValueExprPrimative(PrimativeInt 5)], [], [])
    testParse protoSQL expectedAST

let testWhereExpressionSimpleFunctionCall() =
    let protoSQL = "f?func(Column, '2013-05-05')"
    let astFuncArgs = [ValueExprPrimative (PrimativeLiteral "Column"); ValueExprPrimative(PrimativeString "2013-05-05")]
    let expectedAST = (FromTable("", "", "f"), [ValueExprFunctionCall("func", astFuncArgs)], [], [])
    testParse protoSQL expectedAST

let testAscendingAndOrderByClauses() =
    let protoSQL = @"foo//bar\\baz"
    let expectedAST = (FromTable("", "", "foo"), [], [(Ascending, ("", "", "bar")); (Descending, ("", "", "baz"))], [])
    testParse protoSQL expectedAST

let testSpacedOutCode() =
    let protoSQL = "dbo.[i like  .spacing. $] ?    42 //foo  \n\\\\rawr { x = 42;\n y = '  test ' }"
    let expectedAST = (FromTable("", "dbo", "[i like  .spacing. $]"),
        [ValueExprPrimative(PrimativeInt  42)],
        [(Ascending, ("", "", "foo")); (Descending, ("", "", "rawr"))],
        [SelectExpr("x", ValueExprPrimative(PrimativeInt 42)); SelectExpr("y", ValueExprPrimative(PrimativeString "  test "))]
    )
    testParse protoSQL expectedAST

let testMultitudeOfOperators() =
    let protoSQL = "Person ?x > 5 ?y-3 = 2"

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
    testParse protoSQL expectedAST

let testParenthesisExpression() =
    let protoSQL = "foo?((x / 5) + 1) > bar(y)"

    let divisionAST = ValueExprBinaryOperator("/", ValueExprPrimative(PrimativeLiteral "x"), ValueExprPrimative(PrimativeInt 5))
    let additionaST = ValueExprBinaryOperator("+", divisionAST, ValueExprPrimative(PrimativeInt 1))
    let barCallAST = ValueExprFunctionCall("bar", [ValueExprPrimative(PrimativeLiteral "y")])
    let expectedAST = (FromTable("", "", "foo"), [ValueExprBinaryOperator(">", additionaST, barCallAST)], [], [])

    testParse protoSQL expectedAST

let testLikeOperator() =
    let protoSQL = "foo?x~'bar'"
    let expectedAST = (FromTable("", "", "foo"), [ValueExprBinaryOperator("~", ValueExprPrimative(PrimativeLiteral "x"), ValueExprPrimative(PrimativeString "bar"))], [], [])
    testParse protoSQL expectedAST

let testInnerJoin() =
    let protoSQL = "dbo.Test -> dbo.Test2()"
    let expectedAST = (FromJoins[(("", "dbo", "Test"), JoinType.InnerJoin, ("", "dbo", "Test2"), ("", ""))], [], [], [])
    testParse protoSQL expectedAST

let testOuterJoin() =
    let protoSQL = "dbo.Test => dbo.Test2()"
    let expectedAST = (FromJoins[(("", "dbo", "Test"), JoinType.OuterJoin, ("", "dbo", "Test2"), ("", ""))], [], [], [])
    testParse protoSQL expectedAST

let testCrossJoin() =
    let protoSQL = "dbo.Test +> dbo.Test2()"
    let expectedAST = (FromJoins[(("", "dbo", "Test"), JoinType.CrossJoin, ("", "dbo", "Test2"), ("", ""))], [], [], [])
    testParse protoSQL expectedAST

let testMultiColumns() =
    let protoSQL = "[database].[schema].[table]?[schema].[table].[column] = 42"
    let expectedOperatorAST = 
        ValueExprBinaryOperator("=",
            ValueExprPrimative(PrimativeColumn("[schema]", "[table]", "[column]")),
            ValueExprPrimative(PrimativeInt 42))

    let expectedAST = (FromTable("[database]", "[schema]", "[table]"), [expectedOperatorAST], [], [])
    testParse protoSQL expectedAST

let testOptionalTrailingSelectSemicolon() =
    let protoSQL = "f{x;}"
    let expectedAST = (FromTable("", "", "f"), [], [], [SelectColumn(("", "", "x"))])
    testParse protoSQL expectedAST

let testSimpleBitOfEverything() =
    let protoSQL = "dbo.Foo?5//x{y}"
    let expectedAST = (FromTable("", "dbo", "Foo"), [ValueExprPrimative(PrimativeInt 5)], [(Ascending,("", "", "x"))], [SelectColumn(("", "", "y"))])
    testParse protoSQL expectedAST

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
        testMultiColumns;
        testOptionalTrailingSelectSemicolon;
        testSimpleBitOfEverything
    ]

    let mutable all_correct = true
    for test in tests do
        let success = test()
        if not success then
            all_correct <- false
    
    if all_correct then printn "All tests passed!"