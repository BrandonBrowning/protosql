
module Main

open System
open Common
open Cross
open FParsec
open Grammar
open Optimize
open Parse
open Test

let runPrint p str =
    let result = run p str

    printn str
    printn "---"

    match result with
        | Success(result, _, _) ->
            let sql = cross result
            let optimizedSql = cross <| optimize result

            printfn "Success: %A" result
            printn "---"
            printn sql
            printn "---"
            printn optimizedSql
        
        | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

    printn ""

[<EntryPoint>]
let main args =
    let testing = true
    if testing then
        test()
    else
        let query = "table.name?gt(StartDate,getdate())/StartDate{foo=bar;baz=5.5/3}" // @"customer.Account?gt(StartDate,'2012-05-16'){EmploymentLength=datediff(getdate(), StartDate); Foo=bar+1}"
        runPrint protoSqlParser query

    Console.ReadLine() |> ignore
    0
