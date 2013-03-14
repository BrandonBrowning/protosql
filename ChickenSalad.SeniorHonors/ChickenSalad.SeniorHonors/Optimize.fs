
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
        | ValueExprBinaryOperator(ident, leftExpr, rightExpr) ->
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
                            ValueExprBinaryOperator(ident, optimizedLeft, optimizedRight)
                    | _ -> ValueExprBinaryOperator(ident, optimizedLeft, optimizedRight)
            else
                expr
        | _ -> expr

let optimizeFrom = id

let optimizeWhere = optimizeValueExpr

let optimizeOrderBy = id

let optimizeSelect line =
    match line with
        | SelectExpr(ident, valueExpr) -> SelectExpr(ident, optimizeValueExpr valueExpr)
        | _ -> line

let optimize (from, wheres, orderBys, selects) = 
    let optimizedFrom = optimizeFrom from
    let optimizedWheres = List.map optimizeWhere wheres
    let optimizedOrderBy = List.map optimizeOrderBy orderBys
    let optimizedSelect = List.map optimizeSelect selects

    (optimizedFrom, optimizedWheres, optimizedOrderBy, optimizedSelect)