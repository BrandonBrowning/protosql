
module Main
open System
open FParsec
open ProtoSql

let test p str =
    match run p str with
        | Success(result, _, _)   -> printfn "Success: %A" result
        | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

[<EntryPoint>]
let main args =
    test parser "foo(bar(5, -42.01, 'rawr'), 1)"

    Console.ReadLine() |> ignore
    0