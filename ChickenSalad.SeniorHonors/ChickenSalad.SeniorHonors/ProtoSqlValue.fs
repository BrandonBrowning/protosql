
module ProtoSqlValue
open System
open FParsec
open Parsing

type ProtoSqlValue =
    | ProtoSqlLiteral of string
    | ProtoSqlString of string
    | ProtoSqlInteger of int
    | ProtoSqlFloat of float
    | ProtoSqlFunctionCall of string * ProtoSqlValue list
    
type ProtoSqlValueParser = Parser<ProtoSqlValue, unit>

let protoSqlValueParser, protoSqlValueParserRef = createParserForwardedToRef()

let parseRawIdentifier: Parser<string, unit> = many1Satisfy2 isLetter (fun c -> isLetter c || isDigit c)
let parseRawString = pchar ''' >>. manySatisfy (fun c -> c <> ''') .>> pchar '''
let parseRawInteger = pint32
let parseRawFloat = pfloat

let parseLiteral = parseRawIdentifier |>> ProtoSqlLiteral
let parseString = parseRawString |>> ProtoSqlString
let parseInteger: ProtoSqlValueParser = parseRawInteger |>> ProtoSqlInteger
let parseFloat: ProtoSqlValueParser = parseRawFloat |>> ProtoSqlFloat

let parseNumberOptions = NumberLiteralOptions.AllowFraction ||| NumberLiteralOptions.AllowMinusSign
let parseNumber = 
    numberLiteral parseNumberOptions "number"
        |>> fun x ->
            if x.IsInteger then x.String |> int |> ProtoSqlInteger
            else x.String |> float |> ProtoSqlFloat

let parseIdentOrFunctionCall =
    let argumentCsv = csv protoSqlValueParser
    let funcCall = betweenStr "(" ")" argumentCsv

    parseRawIdentifier .>>. (opt funcCall)
        |>> fun (ident, fcall) -> match fcall with
            | Some args -> ProtoSqlFunctionCall(ident, args)
            | None -> ProtoSqlLiteral(ident)

protoSqlValueParserRef := choice [
    parseIdentOrFunctionCall;
    parseString;
    parseNumber
]