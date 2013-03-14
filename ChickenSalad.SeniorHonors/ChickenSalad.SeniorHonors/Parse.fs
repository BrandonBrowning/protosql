
module Parse

open System
open Common
open FParsec
open Grammar

type Parser = Parser<ProtoSql, unit>

let (protoSqlParser: Parser), protoSqlParserRef = createParserForwardedToRef()

let parseRawIdentifier: Parser<string, unit> = many1Satisfy2 isLetter (fun c -> isLetter c || isDigit c)
let parseRawString = pchar ''' >>. manySatisfy (fun c -> c <> ''') .>> pchar '''
let parseRawInteger = pint32
let parseRawFloat = pfloat

let parseBoolean = (stringReturn "true" true <|> stringReturn "false" false) |>> PrimativeBoolean
let parseLiteral = parseRawIdentifier |>> PrimativeLiteral
let parseString = parseRawString |>> PrimativeString

let parseNumberOptions = NumberLiteralOptions.AllowFraction ||| NumberLiteralOptions.AllowMinusSign
let parseNumber = 
    numberLiteral parseNumberOptions "number"
        |>> fun x ->
            if x.IsInteger then x.String |> int |> PrimativeInt
            else x.String |> float |> PrimativeFloat

let parsePrimative =
    choice [
        parseString;
        parseNumber;
        parseBoolean;
        parseLiteral
    ]

let parseValueExprInternal, parseValueExprInternalRef = createParserForwardedToRef()

let opp = new OperatorPrecedenceParser<_,_,_>()
let parseValueExpr = opp.ExpressionParser

let adjustPosition offset (pos: Position) =
    Position(pos.StreamName, pos.Index + int64 offset,
             pos.Line, pos.Column + int64 offset)

let addInfixOperator str prec assoc =
    let mapping _ left right = ValueExprBinaryOperator(str, left, right)
    let op = InfixOperator(str, getPosition .>> spaces, prec, assoc, (), mapping)

    opp.AddOperator(op)

addInfixOperator "&&" 5 Associativity.Left
addInfixOperator "||" 5 Associativity.Left
addInfixOperator "<" 10 Associativity.Left
addInfixOperator "<=" 10 Associativity.Left
addInfixOperator ">" 10 Associativity.Left
addInfixOperator ">=" 10 Associativity.Left
addInfixOperator "~" 10 Associativity.Left
addInfixOperator "~=" 10 Associativity.Left
addInfixOperator "=" 10 Associativity.Left
addInfixOperator "!=" 40 Associativity.Left
addInfixOperator "%" 10 Associativity.Left
addInfixOperator "*" 10 Associativity.Left
addInfixOperator "/" 10 Associativity.Left
addInfixOperator "+" 20 Associativity.Left
addInfixOperator "-" 20 Associativity.Left

let parseFunctionCall = 
    parseRawIdentifier .>>. (betweenChr '(' ')' <| csv parseValueExpr)
        |>> ValueExprFunctionCall

parseValueExprInternalRef := 
    choice [
        between (chr_ws '(') (ws_chr ')') parseValueExpr
        attempt parseFunctionCall;
        parsePrimative |>> ValueExprPrimative
    ]

opp.TermParser <- (parseValueExprInternal .>> spaces)

let parseWhereCompoundKey =
    csv parseValueExpr
        |> betweenChr '(' ')'

let parseWheres = 
    let parseWhere = parseValueExpr

    (chr '?' >>. (spaces >>. parseWhere)) .>> spaces
        |> many

let parseEscapeBlock = chr '[' >>. many1Satisfy ((<>) ']') .>> chr ']'
let parsePiece = parseRawIdentifier <|> parseEscapeBlock
let parseThreePiece = 
    sepBy1 parsePiece  (chr '.')
        |>> fun pieces -> 
            match pieces.Length with
                | 3 -> (pieces.[0], pieces.[1], pieces.[2])
                | 2 -> ("", pieces.[0], pieces.[1])
                | 1 -> ("", "", pieces.[0])
                | _ -> ("", "", "")
        |> fun p ->
            fun stream ->
                let result = (p stream).Result
                if result = ("", "", "") then
                    Reply(Error, expectedString "Only 1, 2, or 3 parts to column or table identifier")
                else
                    Reply(result)

let parseTable = parseThreePiece
let parseColumn = parseThreePiece

let parseOrderByColumnType = (stringReturn @"_/" Ascending) <|> (stringReturn @"\_" Descending)
let parseOrderBy = parseOrderByColumnType .>>. parseColumn
let parseOrderBys = (parseOrderBy .>> spaces) |> many

let parseSelectExpr = (parseRawIdentifier .>> (spaces .>> chr '=' .>> spaces)) .>>. parseValueExpr
let parseSelect =
    choice [
        (attempt parseSelectExpr) |>> SelectExpr
        parseColumn |>> SelectColumn
    ]

let parseSelects = 
    sepBy parseSelect ((skipNewline <|> skipChar ';') .>> spaces) 
        |> between (chr '{' .>> spaces) (spaces >>. chr '}')
        |> opt
        |>> function
            | Some(x) -> x
            | None -> []

let parseFrom = parseTable
protoSqlParserRef := tuple4 parseFrom (spaces >>. parseWheres) (spaces >>. parseOrderBys) parseSelects .>> eof