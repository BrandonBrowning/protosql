
module Basic
open FParsec
open Parsing

type FooPrimative =
    | FooInt of int
    | FooString of string

type FooExpr =
    | FooExprPrimative of FooPrimative
    | FooExprLiteral of string
    | FooExprCall of string * FooExpr list

type FooLine =
    | FooAssignment of string * FooExpr
    | FooLineCall of FooExpr

type FooCode = FooLine list

let parseRawIdentifier: Parser<string, unit> = many1Satisfy2 isLetter (fun c -> isLetter c || isDigit c)

type FooPrimativeParser = Parser<FooPrimative, unit>

let pfooInt: FooPrimativeParser = pint32 |>> FooInt
let pfooString: FooPrimativeParser = 
    pchar '"' >>. manySatisfy (fun c -> c <> '"') .>> pchar '"'
        |>> FooString

let pfooPrimative = choice [pfooInt; pfooString]

type FooExprParser = Parser<FooExpr, unit>

let pfooExpr, pfooExprRef = createParserForwardedToRef()

let pfooExprPrimative: FooExprParser = pfooPrimative |>> FooExprPrimative
let pfooExprLiteral: FooExprParser = parseRawIdentifier |>> FooExprLiteral
let pfooExprCall: FooExprParser =
    let argumentCsv = csv pfooExpr
    let funcCall = betweenStr "(" ")" argumentCsv
    parseRawIdentifier .>>. funcCall |>> FooExprCall

pfooExprRef := choice[attempt pfooExprCall; pfooExprPrimative; pfooExprLiteral]

let pfooAssignment = (parseRawIdentifier .>> spaces .>> pstring "=" .>> spaces) .>>. pfooExpr |>> FooAssignment
let pfooLineCall = pfooExpr |>> FooLineCall

let pfooLine = choice [attempt pfooAssignment; pfooLineCall]

let pfoo: Parser<FooCode, unit> = many (pfooLine .>> opt newline)