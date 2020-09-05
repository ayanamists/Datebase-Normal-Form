module Closure

open FSharp.Core
open System
open data_type

(*
    一个属性集的闭包
*)
let rec ClosureOfSet (s: String List) (funcD: FuncDependencys) = 
    //printfn "s:%A funcD:%A" s funcD
    let rec Test (s: String List) (funcD: FuncDependencys) = 
        match funcD with
        | [] -> false
        | car :: cdr ->
            let (s1, s2) = car
            if (SetInSet s1 s) && (not (SetInSet s2 s))
            then true
            else Test s cdr
    let rec Simply (s: String List) (funcD: FuncDependencys) =
        match funcD with
        | [] -> s
        | car :: cdr ->
            let (s1, s2) = car
            if SetInSet s1 s
            then Simply (UnionTwoSet s2 s) cdr
            else Simply s cdr
    if not (Test s funcD)
    then s
    else ClosureOfSet (Simply s funcD) funcD

let IsCandidateKey (s : String List) (funcD: FuncDependencys) (rel: Relation) = 
    let closure = ClosureOfSet s funcD
    SetInSet rel s

(*
    函数依赖集的最小集
*)
let MiniCover funcD = 
    let rec FactorAll funcD tar = 
        match funcD with
        | [] -> tar
        | car :: cdr -> 
            let (s1, s2) = car
            FactorAll cdr (List.append (List.fold (fun res x -> (s1, [x]) :: res) [] s2) tar)
    let rec DeleteFromLeft aFuncD restfuncDs = 
        let (left,right) = aFuncD
        let rec Inner leftUnchecked leftChecked = 
            match leftUnchecked with
            | [] -> leftChecked
            | car :: [] ->
                match leftChecked with
                | [] -> [car]
                | _ -> leftChecked
            | car :: cdr ->
                let newClosure = ClosureOfSet cdr (aFuncD :: restfuncDs)
                if SetInSet right newClosure
                then Inner cdr leftChecked
                else Inner cdr (car :: leftChecked)
        (Inner left [])
    let CanRemove aFuncD restfuncDs = 
        let (left, right) = aFuncD
        let newClosure = ClosureOfSet left restfuncDs
        if SetInSet right newClosure
        then true
        else false
    let rec RemoveProcess funcDUnchecked funcDChecked = 
        match funcDUnchecked with
        | [] -> funcDChecked
        | car :: cdr ->
            let (left, right) = car
            let funcDRest = (List.append cdr funcDChecked) 
            let newLeft = DeleteFromLeft car funcDRest
            let newCar = (newLeft, right)
            if List.isEmpty newLeft
            then RemoveProcess cdr funcDChecked
            else
                if CanRemove newCar funcDRest
                then RemoveProcess cdr funcDChecked
                else RemoveProcess cdr (newCar :: funcDChecked)
    let rec MergeAll (funcDs:FuncDependencys) (res:FuncDependencys) = 
        // printfn "%A" funcDs
        match funcDs with
        | [] -> res
        | car :: cdr -> 
            let (left, right) = car
            let newR = 
                List.fold 
                    (fun res x -> 
                        let xLeft, xRight = x
                        if SetEqual xLeft left
                        then UnionTwoSet xRight res
                        else res)
            let newRight = newR right cdr
            MergeAll (List.filter (fun x -> not (SetEqual (fst x) left)) cdr) ((left, newRight) :: res)
    MergeAll (RemoveProcess (FactorAll funcD []) []) []

        

