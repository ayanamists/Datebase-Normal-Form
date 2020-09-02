module _3rd
open System
open FSharp.Core
open data_type
open Closure

let ThreeRdDecomp rel funcDs = 
    let R = AllVar rel
    let miniCover = MiniCover funcDs
    let firstPass = 
        List.fold 
            (fun res x -> 
                let (a, b) = x
                (List.append a b) :: res)
            []
            miniCover

    let secondPass = 
        let rec FindCandidateKey needToFind = 
            match needToFind with
            | [] -> false
            | car :: cdr -> 
                if IsCandidateKey car funcDs R
                then true
                else FindCandidateKey cdr
        if FindCandidateKey firstPass
        then firstPass
        else 
            let NotAppear =
                List.fold 
                    (fun res x -> 
                        let _, second = x
                        List.filter (fun y -> not (List.exists (fun x -> x = y) second)) res)
                    R
                    miniCover
                    
            let rec Loop funcD init = 
                match funcD with
                | [] -> init
                | car :: cdr ->
                    let (left, right) = car
                    let now = UnionTwoSet left init 
                    if IsCandidateKey now miniCover R
                    then now
                    else Loop cdr now
            (Loop miniCover NotAppear) :: firstPass

    let thirdPass = 
        let rec check unChecked _checked = 
            match unChecked with
            | [] -> _checked
            | car :: cdr ->
                if List.exists (fun x -> SetInSet car x) _checked
                then check cdr _checked
                else 
                    check cdr (car :: (List.filter (fun x -> not(SetInSet x car)) _checked))
        check secondPass []
    thirdPass

