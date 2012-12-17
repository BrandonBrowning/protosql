
module Main
open System
open FParsec
open SimpleSql

let test p str =
    match run p str with
        | Success(result, _, _)   -> printfn "Success: %A" result
        | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

[<EntryPoint>]
let main args =
    test simpleSqlParser "GETDATE()"

    Console.ReadLine() |> ignore
    0