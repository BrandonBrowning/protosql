
module Main
open System
open FParsec

type UserState = unit
type P<'t> = Parser<'t, UserState>

let test p str =
    match run p str with
        | Success(result, _, _)   -> printfn "Success: %A" result
        | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let str = pstring
let sstr s = str s >>. spaces
let bracketed p = sstr "[" >>. p .>> sstr "]"
let csv p = sepBy p (sstr ",")

let csvFloat: P<_> = csv pfloat |> bracketed
let csvCsvFloat: P<_> = csv csvFloat |> bracketed

let ws_str s = spaces >>. (str s)
let str_ws s = (str s) .>> spaces

type Json =
    JsonString of string
    | JsonNumber of double
    | JsonBool of bool
    | JsonList of Json list
    | JsonNull
    | JsonObject of Map<string, Json>

type JsonP = P<Json>
let jsonp a = a: JsonP

let stringLiteral = str "\"" >>. manySatisfy (fun c -> c <> '"') .>> str "\""

let jnull = JsonNull |> stringReturn "null" |> jsonp
let jtrue = JsonBool true |> stringReturn "true" |> jsonp
let jfalse = JsonBool false |> stringReturn "false" |> jsonp
let jbool = jtrue <|> jfalse |> jsonp
let jnumber = pfloat |>> JsonNumber |> jsonp
let jstring = stringLiteral |>> JsonString |> jsonp

let listBetweenStrings sOpen sClose p f = 
    let inside = spaces >>. sepBy (p .>> spaces) (str "," .>> spaces)
    between (str sOpen) (str sClose) inside |>> f

let jvalue, jvalueRef = createParserForwardedToRef<Json, unit>()

let jlist = listBetweenStrings "[" "]" jvalue JsonList
let keyValue = stringLiteral .>>. (spaces >>. str ":" >>. spaces >>. jvalue)
let jobject = listBetweenStrings "{" "}" keyValue (Map.ofList >> JsonObject)

jvalueRef := choice [jobject; jlist; jstring; jnumber; jtrue; jfalse; jnull]

let jsonParser = spaces >>. jvalue .>> spaces .>> eof

[<EntryPoint>]
let main args =
    test jobject "{\"obj\": {\"foo\": true}, \"str\": \"foo\", \"num\": -42.3}"
    0