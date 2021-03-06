﻿// Learn more about F# at http://fsharp.org

open System

open data_type
open Closure
open bcnf
open _3rd
[<EntryPoint>]
let main argv =
    // Test1
    let rel = [["A";"B";"C";"D";"E"]]
    let funD = [
        (["A"],["B";"C"])
        (["C"],["D";"E"])
    ]
    let target = ["A"]
    printfn "tes1-bcnf\n%A" (BcnfDecomp rel funD)

    //Test2
    let rel2 = [["A";"B";"C";"D"]]
    let funD2 = [
        (["A";"B"],["C"])
        (["B"],["D"])
        (["C"],["A"])
    ]
    let target2 = ["B";"C"]
    printfn "test2-bcnf\n%A" (BcnfDecomp rel2 funD2)
    printfn "test2-3rd\n%A" (ThreeRdDecomp rel2 funD2)

    // Test3
    let rel3 = [["A";"B";"C";"D";"E";"F";"G";"H";"I";"J";"K";"L";"M"]]
    let funD3 =[
        (["A"],["B";"C";"D";"E"])
        (["E"],["F";"G";"H"])
        (["I"],["J"])
        (["A";"L"],["M"])
        (["A";"I"],["K"])
    ]
    printfn "test4-bcnf\n%A" (BcnfDecomp rel3 funD3)
    printfn "test4-3rd\n%A" (ThreeRdDecomp rel3 funD3)

    // Test4
    let rel4 = [["A";"B";"C";"D";"E";"F";"G";"H"]]
    let funcD4 = [
        (["A";"C"], ["G"])
        (["D"], ["E";"G"])
        (["B";"C"], ["D"])
        (["G";"C"], ["B";"D"])
        (["A";"C";"D"], ["B"])
        (["C";"E"], ["A";"G"])
    ]
    printfn "test4 \n %A" (MiniCover funcD4)

    // Test5
    let rel5 = [["A";"B";"C"]]
    let funcD5 = [
        (["A"], ["B";"C"])
        (["B"], ["C"])
        (["A"], ["B"])
        (["A";"B"], ["C"])
    ]
    printfn "test5 \n %A" (MiniCover funcD5)

    // Test6
    let rel6 = [["A";"B";"C";"D";"E"]]
    let funcD6 = [
        (["A"],["B";"C"])
        (["C";"D"],["E"])
        (["B"],["D"])
        (["E"],["A"])
    ]
    printfn "test6 \n %A" (BcnfDecomp rel6 funcD6)
    printfn "test6 mini\n %A" (MiniCover funcD6)

    // Test7
    let rel7 = [["A";"B";"C";"D";"E";"F"]]
    let funcD7 = [
        (["A"],["B";"C";"D"])
        (["B";"C"],["D";"E"])
        (["B"],["D"])
        (["D"],["A"])
    ]
    printfn "test7 bcnf \n %A" (BcnfDecomp rel7 funcD7)
    printfn "test7 3rd\n %A" (ThreeRdDecomp rel7 funcD7)
    printfn "test7 mini\n %A" (MiniCover funcD7)

    0 // return an integer exit code
