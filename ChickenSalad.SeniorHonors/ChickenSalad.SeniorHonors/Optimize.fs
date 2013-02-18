
module Optimize

open System
open System.Text
open Grammar

let isOptimizableOperator ident =
    match ident with
        | "+" | "-" | "*" | "/" -> true
        | _ -> false

let duplicateString n (str: string) =
    let builder = new StringBuilder()

    for i = 1 to n do
        builder.Append(str) |> ignore

    builder.ToString()

let optimizedAdd leftPrim rightPrim =
    match (leftPrim, rightPrim) with
        | (PrimativeInt left, PrimativeInt right) -> left + right |> PrimativeInt |> Some
        | (PrimativeFloat left, PrimativeFloat right) -> left + right |> PrimativeFloat |> Some
        | (PrimativeString left, PrimativeString right) -> left + right |> PrimativeString |> Some
        | (PrimativeInt left, PrimativeFloat right) -> float left + right |> PrimativeFloat |> Some
        | (PrimativeFloat left, PrimativeInt right) -> left + float right |> PrimativeFloat |> Some
        | _ -> None

let optimizedSub leftPrim rightPrim =
    match (leftPrim, rightPrim) with
        | (PrimativeInt left, PrimativeInt right) -> left - right |> PrimativeInt |> Some
        | (PrimativeFloat left, PrimativeFloat right) -> left - right |> PrimativeFloat |> Some
        | (PrimativeInt left, PrimativeFloat right) -> float left - right |> PrimativeFloat |> Some
        | (PrimativeFloat left, PrimativeInt right) -> left - float right |> PrimativeFloat |> Some
        | _ -> None

let optimizedMul leftPrim rightPrim =
    match (leftPrim, rightPrim) with
        | (PrimativeInt left, PrimativeInt right) -> left * right |> PrimativeInt |> Some
        | (PrimativeFloat left, PrimativeFloat right) -> left * right |> PrimativeFloat |> Some
        | (PrimativeString left, PrimativeInt right) -> duplicateString right left |> PrimativeString |> Some
        | (PrimativeInt left, PrimativeString right) -> duplicateString left right |> PrimativeString |> Some
        | (PrimativeInt left, PrimativeFloat right) -> float left * right |> PrimativeFloat |> Some
        | (PrimativeFloat left, PrimativeInt right) -> left * float right |> PrimativeFloat |> Some
        | _ -> None

let optimizedDiv leftPrim rightPrim =
    match (leftPrim, rightPrim) with
        | (PrimativeInt left, PrimativeInt right) -> left / right |> PrimativeInt |> Some
        | (PrimativeFloat left, PrimativeFloat right) -> left / right |> PrimativeFloat |> Some
        | (PrimativeInt left, PrimativeFloat right) -> float left / right |> PrimativeFloat |> Some
        | (PrimativeFloat left, PrimativeInt right) -> left / float right |> PrimativeFloat |> Some
        | _ -> None

let getOptimizeOpFunc ident =
    match ident with
        | "+" -> optimizedAdd
        | "-" -> optimizedSub
        | "*" -> optimizedMul
        | "/" -> optimizedDiv
        | _ -> new ArgumentException("Could not match a function to the given argument") |> raise

let rec optimizeValueExpr expr =
    match expr with
        | ValueExprBinOp(ident, leftExpr, rightExpr) ->
            if isOptimizableOperator ident then
                let optimizedLeft = optimizeValueExpr leftExpr
                let optimizedRight = optimizeValueExpr rightExpr
                
                match (optimizedLeft, optimizedRight) with
                    | (ValueExprPrimative leftPrim, ValueExprPrimative rightPrim) ->
                        let optimizeFunc = getOptimizeOpFunc ident
                        let result = optimizeFunc leftPrim rightPrim
                        if Option.isSome result then
                            Option.get result |> ValueExprPrimative
                        else
                            ValueExprBinOp(ident, optimizedLeft, optimizedRight)
                    | _ -> ValueExprBinOp(ident, optimizedLeft, optimizedRight)
            else
                expr
        | _ -> expr

let optimizeFrom = id

let optimizeWhere line =
    match line with
        | WhereValueExpr valueExpr -> optimizeValueExpr valueExpr |> WhereValueExpr
        | _ -> line

let optimizeOrderBy = id

let optimizeSelectLine line =
    match line with
        | SelectLineExpr(ident, valueExpr) -> SelectLineExpr(ident, optimizeValueExpr valueExpr)
        | _ -> line

let optimizeSelect = List.map optimizeSelectLine

let optimize query = 
    match query with
        Query(from, wheres, orderBy, select) ->
            let optimizedFrom = optimizeFrom from
            let optimizedWheres = Option.bind (List.map optimizeWhere >> Option.Some) wheres
            let optimizedOrderBy = Option.bind (optimizeOrderBy >> Option.Some) orderBy
            let optimizedSelect = Option.bind (optimizeSelect >> Option.Some) select

            Query(optimizedFrom, optimizedWheres, optimizedOrderBy, optimizedSelect)