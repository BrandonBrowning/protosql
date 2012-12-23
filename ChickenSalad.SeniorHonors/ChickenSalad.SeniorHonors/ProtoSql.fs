
module ProtoSql
open System
open FParsec
open Parsing

type ProtoSql =
    | ProtoSqlLiteral of string
    | ProtoSqlString of string
    | ProtoSqlInteger of int
    | ProtoSqlFloat of float
    | ProtoSqlFunctionCall of string * ProtoSql list
    
type ProtoSqlParser = Parser<ProtoSql, unit>

let parser, parserRef = createParserForwardedToRef()

let parseRawIdentifier: Parser<string, unit> = many1Satisfy2 isLetter (fun c -> isLetter c || isDigit c)
let parseRawString = pchar ''' >>. manySatisfy (fun c -> c <> ''') .>> pchar '''
let parseRawInteger = pint32
let parseRawFloat = pfloat

let parseLiteral = parseRawIdentifier |>> ProtoSqlLiteral
let parseString = parseRawString |>> ProtoSqlString
let parseInteger: ProtoSqlParser = parseRawInteger |>> ProtoSqlInteger
let parseFloat: ProtoSqlParser = parseRawFloat |>> ProtoSqlFloat

let parseNumberOptions = NumberLiteralOptions.AllowFraction ||| NumberLiteralOptions.AllowMinusSign
let parseNumber = 
    numberLiteral parseNumberOptions "number"
        |>> fun x ->
            if x.IsInteger then x.String |> int |> ProtoSqlInteger
            else x.String |> float |> ProtoSqlFloat

let parseFunctionCall =
    let argumentCsv = csv parser
    let funcCall = betweenStr "(" ")" argumentCsv

    parseRawIdentifier .>>. funcCall |>> ProtoSqlFunctionCall

parserRef := choice [
    parseFunctionCall;
    parseString;
    parseNumber;
    parseLiteral
]