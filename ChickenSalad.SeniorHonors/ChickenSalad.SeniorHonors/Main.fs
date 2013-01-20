
module Main
open System
open FParsec
open ProtoSqlParser

[<EntryPoint>]
let main args =
    let result = run protoSqlParser  "foo(3, 1, a)"

    match result with
        | Success(result, _, _)   -> printfn "Success: %A" result
        | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

    Console.ReadLine() |> ignore
    0