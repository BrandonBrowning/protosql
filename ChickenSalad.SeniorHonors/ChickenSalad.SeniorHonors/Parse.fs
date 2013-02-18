﻿
module Parse

open System
open Common
open FParsec
open Grammar

type Parser = Parser<ProtoSql, unit>

let protoSqlParser, protoSqlParserRef = createParserForwardedToRef()

let parseRawIdentifier: Parser<string, unit> = many1Satisfy2 isLetter (fun c -> isLetter c || isDigit c)
let parseRawString = pchar ''' >>. manySatisfy (fun c -> c <> ''') .>> pchar '''
let parseRawInteger = pint32
let parseRawFloat = pfloat

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
        parseLiteral
    ]

let parseValueExprInternal, parseValueExprInternalRef = createParserForwardedToRef()

let opp = new OperatorPrecedenceParser<_,_,_>()

let adjustPosition offset (pos: Position) =
    Position(pos.StreamName, pos.Index + int64 offset,
             pos.Line, pos.Column + int64 offset)

let addInfixOperator str prec assoc mapping =
    let op = InfixOperator(str, getPosition .>> spaces, prec, assoc, (),
                           fun opPos leftTerm rightTerm ->
                               mapping
                                   (adjustPosition -str.Length opPos)
                                   leftTerm rightTerm)
    opp.AddOperator(op)

addInfixOperator "+" 1 Associativity.Left (fun opPos leftTerm rightTerm -> ValueExprBinOp("+", leftTerm, rightTerm))

parseValueExprInternalRef := (
    let argumentCsv = csv parseValueExprInternal
    let funcCall = betweenStr "(" ")" argumentCsv

    parsePrimative .>>. (opt funcCall)
        |>> fun (prim, args) -> match prim with
            | PrimativeLiteral(str) -> match args with
                | Some(args) -> ValueExprFCall (str, args)
                | _ -> ValueExprPrimative prim
            | _ -> ValueExprPrimative prim
    )

opp.TermParser <- parseValueExprInternal

let parseValueExpr = opp.ExpressionParser

let parseWhereID =
    choice [
        // NOTE: Seems duplicate logic with valueExprFcall implementation
        //       Should it be a tuple, or share the logic?
        csv parseValueExpr |> betweenChr '(' ')' |>> WhereIDComposite

        // NOTE: The <|> below is essentially my need for a non-literal primative
        //       Should I differentiate between them in the grammar?
        (parseNumber <|> parseString) |>> WhereIDSimple;
    ]

let parseWhere = 
    chr '?' >>. choice [
        parseWhereID |>> WhereID
        parseValueExpr |>> WhereValueExpr
    ] |> many1

let parseEscapeBlock = chr '[' >>. many1Satisfy ((<>) ']') .>> chr ']'
let parsePiece = parseRawIdentifier <|> parseEscapeBlock
let parseThreePiece = sepBy1 parsePiece  (chr '.') |>> 
        fun pieces -> match pieces.Length with
            | 3 -> (pieces.[0], pieces.[1], pieces.[2])
            | 2 -> ("", pieces.[0], pieces.[1])
            | 1 -> ("", "", pieces.[0])
            | _ -> ("", "", "")

let parseTable = parseThreePiece
let parseColumn = parseThreePiece

let parseOrderByColumnType = (charReturn '/' Ascending) <|> (charReturn '\\' Descending)
let parseOrderByColumn = parseOrderByColumnType .>>. parseColumn
let parseOrderBy = parseOrderByColumn |> many1

let parseSelectLineExpr = (parseRawIdentifier .>> (spaces .>> chr '=' .>> spaces)) .>>. parseValueExpr
let parseSelectLine = 
    choice [
        (attempt parseSelectLineExpr) |>> SelectLineExpr
        parseColumn |>> SelectLineColumn
    ]

let parseSelect: Parser<Select> = 
    sepBy1 parseSelectLine ((skipNewline <|> skipChar ';') .>> spaces) 
        |> between (chr '{' .>> spaces) (spaces >>. chr '}')

let parseFrom = parseTable

protoSqlParserRef := tuple4 parseFrom (opt parseWhere) (opt parseOrderBy) (opt parseSelect) |>> Query