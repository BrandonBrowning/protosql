
module JSON
open FParsec
open Parsing

type Json =
    JsonString of string
    | JsonNumber of double
    | JsonBool of bool
    | JsonList of Json list
    | JsonNull
    | JsonObject of Map<string, Json>

type JsonP = Parser<Json, unit>
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