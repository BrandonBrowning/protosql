
module Main
open System
open FParsec

type UserState = unit
type Parser<'t> = Parser<'t, UserState>

let test p str =
    match run p str with
        | Success(result, _, _)   -> printfn "Success: %A" result
        | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let floatsInBrackets: Parser<float list> = between (pstring "[") (pstring "]") pfloat |> many
let floatsWithCommas: Parser<float list> = pstring "[" >>. sepBy pfloat (pstring "," .>> spaces) .>> pstring "]"

[<EntryPoint>]
let main args =
    test floatsInBrackets "[1][2]"
    test floatsWithCommas "[1, 2]"
    0