module data_type
open FSharp.Core
open System

type Relation = List<String>
type Relations = List<Relation> 

// 函数依赖的前项 * 函数依赖的后项，
// 如 (["A"; "B"; "C"], ["D"]) 意为函数依赖关系 ABC -> D
type FuncDependency = List<String> * List<String>
type FuncDependencys = List<FuncDependency>
type Input = Relations * FuncDependencys

(*
    在一个关系模式中出现过的所有变量
*)
let AllVar relations = 
    let rec Inner (rel:Relations) (tar:String List) = 
        if rel.IsEmpty
        then 
            tar
        else
            let this_rel = rel.Head
            if this_rel.IsEmpty
            then 
                Inner rel.Tail tar
            else 
                let this_var = this_rel.Head
                if List.exists (fun x -> x = this_var) tar
                then Inner (this_rel.Tail :: rel.Tail) tar
                else Inner (this_rel.Tail :: rel.Tail) (this_var :: tar)
    List.sort(Inner relations [])

(*
    求两个属性集的并集
*)
let rec UnionTwoSet (a:String List) (b:String List) = 
    if a.IsEmpty
    then b
    else 
        let now = a.Head
        if List.exists (fun x -> x = now) b
        then UnionTwoSet a.Tail b
        else UnionTwoSet a.Tail (now :: b)

(*
    求两个属性集的交集
*)
let rec InterTwoSet (a:String List) (b:String List) = 
    if a.IsEmpty 
    then []
    else 
        let now = a.Head
        if List.exists (fun x -> x = now) b
        then now :: (InterTwoSet a.Tail) b
        else (InterTwoSet a.Tail b)

(*
    求 A属性集 - B属性集
*)
let rec DelBFromA (a:String List) (b: String List) = 
    match b with
    | [] -> a
    | car :: cdr -> DelBFromA (List.filter (fun x -> not (x = car)) a) cdr 

(*
    sa 是否属于 sb, 例如 (SetInSet ["A";"B"] ["A"; "B"; "C"]) = True
    这里是包含关系，不要求真包含
*)
let rec SetInSet (sa:String List) (sb: String List) = 
    if sa.IsEmpty
    then true
    else 
        let car = sa.Head
        let cdr = sa.Tail
        if List.exists (fun x -> x = car) sb
        then SetInSet cdr sb
        else false

let IsTrivial funcD = 
    match funcD with
    | (a, b) -> (SetInSet a b) && (SetInSet b a)

let SetEqual sa sb = 
    (SetInSet sa sb) && (SetInSet sb sa)
            
