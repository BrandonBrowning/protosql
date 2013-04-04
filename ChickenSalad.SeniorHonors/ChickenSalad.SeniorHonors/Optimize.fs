
// Note: Only for senior capstone project

module Optimize

open System
open System.Linq
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
                    | _ ->
                        ValueExprBinaryOperator(ident, optimizedLeft, optimizedRight)
            else
                expr
        | _ ->
            expr

(*
// NOTE: Progress pending new three-part primative
// NOTE: If you eliminate the only join clause, the left table should remain as FROM
let rec valueExprColumnReferences valueExpr =
    seq {
        match valueExpr with
            | ValueExprPrimative(PrimativeColumn col) ->
                yield col
            | ValueExprBinaryOperator(op, l, r) ->
                yield! valueExprColumnReferences l
                yield! valueExprColumnReferences r
            | ValueExprFunctionCall(f, args) ->
                yield! args |> Seq.map valueExprColumnReferences |> Seq.concat
            | _ -> ignore ()
    }

let whereColumnReferences = valueExprColumnReferences
let orderByColumnReferences = snd
let selectColumnReferences select =
    seq {
        match select with
            | SelectColumn col -> yield col
            | SelectExpr(_, expr) -> yield! valueExprColumnReferences expr
    }

let columnTableReferences columnReferences =
    seq {
        for (_, table, _) in columnReferences do
            yield table
    } |> Seq.filter (not << String.IsNullOrEmpty)

let filterFromClause columnReferences fromClause =
        match fromClause with
            | FromJoins(joins) ->
                let remainingJoins = 
                    seq {
                        let colTableRefs = columnTableReferences columnReferences

                        for join in joins do
                            let fromTable, joinType, toTable, columns = join
                            let _, targetTable, _ = toTable
                            if Seq.exists ((=) targetTable) colTableRefs then
                                yield join
                    } |> List.ofSeq

                if remainingJoins.Length = 0 then
                    
                else
                    FromJoins(remainingJoins)
            | x -> x

let queryColumnReferences (from, wheres, orderBys, selects) =
    seq {
        yield! Seq.map whereColumnReferences wheres |> Seq.concat
        yield! Seq.map orderByColumnReferences orderBys
        yield! Seq.map selectColumnReferences selects |> Seq.concat
    }

let optimizeFrom query =
    let from, wheres, orderBys, selects = query

    filterFromClause (queryColumnReferences query) from
*)

let optimizeFrom (from, wheres, orderBys, selects) = from 

let optimizeWhere = optimizeValueExpr

let optimizeOrderBy = id

let optimizeSelect line =
    match line with
        | SelectExpr(ident, valueExpr) -> SelectExpr(ident, optimizeValueExpr valueExpr)
        | _ -> line

let optimize query = 
    let from, wheres, orderBys, selects = query

    let optimizedFrom = optimizeFrom query
    let optimizedWheres = List.map optimizeWhere wheres
    let optimizedOrderBy = List.map optimizeOrderBy orderBys
    let optimizedSelect = List.map optimizeSelect selects

    (optimizedFrom, optimizedWheres, optimizedOrderBy, optimizedSelect)