
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

let parseWhereID =
    choice [
        // NOTE: Seems duplicate logic with valueExprFcall implementation
        //       Should it be a tuple, or share the logic?
        csv parseValueExpr |> betweenChr '(' ')' |>> WhereIDComposite

        // NOTE: The <|> below is essentially my need for a non-literal primative
        //       Should I differentiate between them in the grammar?
        (parseNumber <|> parseString) |>> WhereIDSimple;
    ]

let parseWhere = chr '?' >>. choice [
        parseWhereID |>> WhereID
        parseValueExpr |>> WhereValueExpr
    ]

let parseEscapeBlock = chr '[' >>. many1Satisfy ((<>) ']') .>> chr ']'
let parsePiece = parseRawIdentifier <|> parseEscapeBlock
let parseThreePiece = sepBy1 parsePiece  (chr '.') |>> 
    fun pieces ->
        match pieces.Length with
            | 3 -> (pieces.[0], pieces.[1], pieces.[2])
            | 2 -> ("", pieces.[0], pieces.[1])
            | 1 -> ("", "", pieces.[0])
            | _ -> ("", "", "")

let parseTable = parseThreePiece |>> Table
let parseColumn = parseThreePiece |>> Column

let parseOrderByColumnType = (charReturn '/' Ascending) <|> (charReturn '\\' Descending)
let parseOrderByColumn = parseOrderByColumnType .>>. parseColumn
let parseOrderBy = parseOrderByColumn |> many

let parseSelectLineExpr = (parseRawIdentifier .>> (spaces .>> chr '=' .>> spaces)) .>>. parseValueExpr
let parseSelectLine = 
    choice [
        parseSelectLineExpr |>> SelectLineExpr
        parseColumn |>> SelectLineColumn
    ]

let parseSelect: Parser<Select> = sepBy1 parseSelectLine ((skipNewline <|> skipChar ';') .>> spaces) |> between (chr '{' .>> spaces) (spaces >>. chr '}')

let queryExceptDataset where orderby select = Query (("", "dbo", "foo") |> Table, where, orderby, Option.Some select)

protoSqlParserRef := parseSelect