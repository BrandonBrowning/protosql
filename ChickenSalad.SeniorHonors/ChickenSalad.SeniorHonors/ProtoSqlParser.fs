
module ProtoSqlParser
open System
open FParsec
open Parsing
open ProtoSql

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

let parseValueExpr, parseValueExprRef = createParserForwardedToRef()

parseValueExprRef := (
    let argumentCsv = csv parseValueExpr
    let funcCall = betweenStr "(" ")" argumentCsv

    parsePrimative .>>. (opt funcCall)
        |>> fun (prim, args) -> match prim with
            | PrimativeLiteral(str) -> match args with
                | Some(args) -> ValueExprFCall (FCall(str, args))
                | _ -> ValueExprPrimative prim
            | _ -> ValueExprPrimative prim)

protoSqlParserRef := parseValueExpr