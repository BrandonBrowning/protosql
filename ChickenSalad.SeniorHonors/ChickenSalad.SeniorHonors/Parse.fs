
module Parse

open System
open Common
open FParsec
open Grammar

type Parser = Parser<ProtoSql, unit>

let (protoSqlParser: Parser), protoSqlParserRef = createParserForwardedToRef()

let threePieceLength (a, b, c) =
    [a; b; c]
        |> List.filter (not << String.IsNullOrEmpty)
        |> List.length

let parseEscapeBlock = 
    chr '[' >>. many1Satisfy ((<>) ']') .>> chr ']'
        |>> fun s -> "[" + s + "]"

let parseRawIdentifier = many1Satisfy2 isLetter (fun c -> isLetter c || isDigit c)
let parsePiece =  parseEscapeBlock <|> parseRawIdentifier
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
                let reply = p stream

                if reply.Status <> ReplyStatus.Ok then
                    Reply(Error, unexpectedString "Table or Column parsing error")
                else 
                    match reply.Result with
                        | ("", "", "") -> Reply(Error, unexpectedString "Column or Table identifier should contain 1, 2, or 3 parts")
                        | result       -> Reply(result)

let parseJoinTwoPiece = 
    chr '(' >>. spaces >>. sepBy parseRawIdentifier (chr ',' .>> spaces) .>> spaces .>> chr ')'
        |>> fun pieces -> 
            match pieces.Length with
                | 2 -> (pieces.[0], pieces.[1])
                | 1 -> (pieces.[0], "")
                | 0 -> ("", "")
                | n -> (null, n.ToString())
        |> fun p ->
            fun stream ->
                let reply = p stream

                if reply.Status = ReplyStatus.Ok then
                    Reply(reply.Result)
                else
                    Reply(Error, expectedString <| sprintf "Join two-piece parsing error")

let flattenIdentifier (a, b, c) =
    [a; b; c]
        |> Seq.filter (not << String.IsNullOrEmpty)
        |> sjoin "."

let parseLiteralOrColumn =
    parseThreePiece
        <?> "literal"
        |>> fun triple ->
            if threePieceLength triple = 1 then
                let _, _, literal = triple
                PrimativeLiteral literal
            else
                PrimativeColumn triple

let parseRawString = pchar ''' >>. manySatisfy (fun c -> c <> ''') .>> pchar '''
let parseRawInteger = pint32
let parseRawFloat = pfloat 
let parseBoolean = (stringReturn "true" true <|> stringReturn "false" false) |>> PrimativeBoolean <?> "bool"
let parseString = parseRawString |>> PrimativeString <?> "string"

let parseNumberOptions = NumberLiteralOptions.AllowFraction ||| NumberLiteralOptions.AllowMinusSign
let parseNumber = 
    numberLiteral parseNumberOptions "number"
        |>> (fun x ->
            if x.IsInteger then x.String |> int |> PrimativeInt
            else x.String |> float |> PrimativeFloat) <?> "number"

let parsePrimative =
    choice [
        parseString;
        parseNumber;
        parseBoolean;
        parseLiteralOrColumn
    ] <?> "primative"

let parseValueExprInternal, parseValueExprInternalRef = createParserForwardedToRef()

let opp = new OperatorPrecedenceParser<_,_,_>()
let parseValueExpr = opp.ExpressionParser

let adjustPosition offset (pos: Position) =
    Position(pos.StreamName, pos.Index + int64 offset,
             pos.Line, pos.Column + int64 offset)

let addInfixOperator str prec assoc =
    let mapping _ left right = ValueExprBinaryOperator(str, left, right)
    let parseOperator = anyOf "&|<=>~!%*/+-"
    let op = InfixOperator(str, notFollowedBy (chr '/') .>> spaces, prec, assoc, (), mapping)

    opp.AddOperator(op)

addInfixOperator "&&" 5 Associativity.Left
addInfixOperator "||" 5 Associativity.Left
addInfixOperator "<" 10 Associativity.Left
addInfixOperator "<=" 10 Associativity.Left
addInfixOperator ">" 10 Associativity.Left
addInfixOperator ">=" 10 Associativity.Left
addInfixOperator "~" 10 Associativity.Left
addInfixOperator "!~" 10 Associativity.Left
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

opp.TermParser <- parseValueExprInternal .>> spaces

let parseWhereCompoundKey =
    csv parseValueExpr
        |> betweenChr '(' ')'

let parseWheres = 
    let parseWhere = parseValueExpr <?> "where expression"

    (chr '?' >>. (spaces >>. parseWhere)) .>> spaces
        |> many
        <?> "where clause"

let parseTable = parseThreePiece
let parseColumn = parseThreePiece

let parseOrderByColumnType = (stringReturn @"//" Ascending) <|> (stringReturn @"\\" Descending) <?> "order-by type"
let parseOrderBy = (parseOrderByColumnType .>> spaces) .>>. parseColumn
let parseOrderBys = 
    parseOrderBy .>> spaces
        |> many
        <?> "order-by clause"

let parseSelectExpr = (parseRawIdentifier .>> (spaces .>> chr '=' .>> spaces)) .>>. parseValueExpr
let parseSelect =
    choice [
        (attempt parseSelectExpr) |>> SelectExpr
        parseColumn |>> SelectColumn
    ]

let parseSelects = 
    parseSelect
        |> seperate (chr ';' .>> spaces)
        |> between (chr '{' .>> spaces) (spaces >>. chr '}')
        |> opt
        |>> function
            | Some(x) -> x
            | None -> []
        <?> "select clause"

let parseJoinArrow = 
    stringReturn "->" InnerJoin
    <|> stringReturn "=>" OuterJoin
    <|> stringReturn "+>" CrossJoin
    <?> "join arrow"

let parseJoinColumns = 
    choice [
        parseJoinTwoPiece;
        stringReturn "" ("", "")
    ] <?> "join column list"

let parseJoin = tuple4 (parseTable .>> spaces) (parseJoinArrow .>> spaces) (parseTable .>> spaces) parseJoinColumns

let parseFrom =
    choice [
        many1 parseJoin <?> "join list" |> attempt |>> FromJoins;
        parseTable <?> "table name" |>> FromTable
    ] <?> "from clause"

protoSqlParserRef := 
    spaces >>.
        tuple4 (parseFrom .>> spaces) (parseWheres .>> spaces) (parseOrderBys .>> spaces) parseSelects
        .>> spaces
        .>> eof