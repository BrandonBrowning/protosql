
module Main
open System
open FParsec
open Basic

[<EntryPoint>]
let main args =
    let result = run pfoo "x = foo(5\r\ny = x\r\nprint(y)"

    match result with
        | Success(result, _, _)   -> printfn "Success: %A" result
        | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

    Console.ReadLine() |> ignore
    0