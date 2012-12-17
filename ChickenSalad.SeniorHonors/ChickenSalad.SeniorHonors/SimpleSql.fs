
module SimpleSql
open System
open FParsec
open Parsing

type SSTablePart = 
    Unspecified
    | Specified of string

type SSResult = 
    | SSInteger of int
    | SSFloat of double
    | SSString of string
    | SSFunction of string * (SSResult list)

type SimpleSql =
    | SSTable of string list

let identifier = many1Satisfy2 isLetter (fun c -> isLetter c || isDigit c)

let ssinteger = pint32
let ssfloat = pfloat
let ssstring = pchar ''' >>. manySatisfy (fun c -> c <> ''') .>> pchar '''

let ssvalue, ssvalueRef = createParserForwardedToRef<SSResult, unit>()

let ssargumentList = pchar '(' >>. sepBy (ssvalue .>> spaces) (str "," .>> spaces) .>> pchar ')'
let ssfunction = identifier .>>. ssargumentList |>> SSFunction

let sstable: Parser<SimpleSql> =
    let parseNaked = manySatisfy (fun c -> c = ' ' || c = '_' || c = '.' || isDigit c || isLetter c)
    let parseBracketed = betweenStr "[" "]" parseNaked
    let parsePart = parseBracketed <|> parseNaked

    sepBy1 parsePart (str ".") |>> SSTable

ssvalueRef := choice [
    ssfunction;
    ssstring |>> SSString;
    ssinteger |>> SSInteger;
    ssfloat |>> SSFloat
]

let simpleSqlParser = ssvalue