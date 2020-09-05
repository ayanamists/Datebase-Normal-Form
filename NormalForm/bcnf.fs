module bcnf
open Closure
open data_type
open FSharp.Core

let rec BcnfDecomp (rels:Relations) (funcD:FuncDependencys) = 
    let rec DecompRel (rel:Relation) (now: FuncDependencys) = 
        //printfn "%A %A" rel now
        match now with
        | [] -> [rel]
        | car :: cdr ->
            let (s1, s2) = car
            let closure = ClosureOfSet s1 funcD
            //printfn "c:%A -> %A" s1 closure
            if (SetInSet rel closure) || (IsTrivial car)
            then DecompRel rel cdr
            else
                let r1 = InterTwoSet s1 rel
                match r1 with
                | [] -> DecompRel rel cdr
                | _ -> 
                    let closure1 = ClosureOfSet r1 funcD
                    let r2 = DelBFromA (InterTwoSet closure1 rel) r1
                    match r2 with
                    | [] -> DecompRel rel cdr
                    | _ -> 
                        let newR1 = DelBFromA rel r2
                        let newR2 = UnionTwoSet r1 r2
                        BcnfDecomp [newR1;newR2] funcD
    let rec Inner rels funcD res = 
        match rels with
        | [] -> res
        | car :: cdr -> Inner cdr funcD (List.append (DecompRel car funcD) res)
    Inner rels funcD []
