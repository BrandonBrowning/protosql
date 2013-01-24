
module Main
open System
open FParsec
open ProtoSqlParser

[<EntryPoint>]
let main args =
    let result = run protoSqlParser @"{ foo = bar(baz); booze = bajangles }"

    match result with
        | Success(result, _, _)   -> printfn "Success: %A" result
        | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

    Console.ReadLine() |> ignore
    0