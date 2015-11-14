namespace FSharpx.Reflection.Tests

//make sure it is faster!
module PerformanceTest =

    open NUnit.Framework
    open Microsoft.FSharp.Reflection

    open FSharpx.Reflection

    let [<Literal>] NumRepeats = 400000
    let [<Literal>] ExpectedImprovement = 1L //conservative

    type MyRecord =
        { S : string
          I : int
          F : float }

    let repeat f = 
        let stopwatch = new System.Diagnostics.Stopwatch()
        stopwatch.Start()
        for i in 1..NumRepeats do f i |> ignore
        stopwatch.Stop()
        stopwatch.ElapsedMilliseconds

    let fastRecordCtor = FSharpValue.PreComputeRecordConstructorFast typeof<MyRecord>
    let standardRecordCtor = FSharpValue.PreComputeRecordConstructor typeof<MyRecord>

    let fastRecordReader = FSharpValue.PreComputeRecordReaderFast typeof<MyRecord>
    let standardRecordReader = FSharpValue.PreComputeRecordReader typeof<MyRecord>

    [<Test>]
    let ``should construct record faster than F# reflection``() =
        let fast = repeat (fun i -> fastRecordCtor [| "2"; i; 3. |] :?> MyRecord)
        let standard = repeat (fun i -> standardRecordCtor [| "2"; i; 3. |] :?> MyRecord)
        printf "FSharpx is %ix faster" (standard/fast)
        Assert.True(fast < standard/ExpectedImprovement)

    [<Test>]
    let ``should read record faster than F# reflection``() =
        let fast = repeat (fun i -> fastRecordReader { S = "2"; I = i; F = 3.0 })
        let standard = repeat (fun i -> standardRecordReader { S = "2"; I = i; F = 3.0 })
        printf "FSharpx is %ix faster" (standard/fast)
        Assert.True(fast < standard/ExpectedImprovement)

    type MyUnion =
        | Empty
        | One of int
        | Two of string * int

    let unionCases = FSharpType.GetUnionCases typeof<MyUnion>

    let fastUnionCtor = FSharpValue.PreComputeUnionConstructorFast unionCases.[2]
    let standardUnionCtor = FSharpValue.PreComputeUnionConstructor unionCases.[2]

    let fastUnionReader = FSharpValue.PreComputeUnionReaderFast unionCases.[2]
    let standardUnionReader = FSharpValue.PreComputeUnionReader unionCases.[2]

    let standardTagReader = FSharpValue.PreComputeUnionTagReader typeof<MyUnion>
    let fastTagReader = FSharpValue.PreComputeUnionTagReaderFast typeof<MyUnion>

    [<Test>]
    let ``should construct 2-case union faster than F# reflection``() =
        let fast = repeat (fun i -> fastUnionCtor [| "3"; i |] :?> MyUnion)
        let standard = repeat (fun i -> standardUnionCtor [| "3"; i |] :?> MyUnion)
        printf "Fsharpx is %ix faster" (standard/fast)
        Assert.True(fast < standard/ExpectedImprovement)

    [<Test>]
    let ``should read 2-case union faster than F# reflection``() =
        let fast = repeat (fun i -> fastUnionReader (Two ("s",i)))
        let standard = repeat (fun i -> standardUnionReader (Two ("s",i)))
        printf "FSharpx is %ix faster" (standard/fast)
        Assert.True(fast < standard/ExpectedImprovement)

    [<Test>]
    let ``should read union tags faster than F# reflection`` () =
        let fast = repeat (fun i -> fastTagReader (Two ("s",i)))
        let standard = repeat (fun i -> standardTagReader (Two ("s",i)))
        printf "FSharpx is %ix faster" (standard/fast)
        Assert.True(fast < standard/ExpectedImprovement)


    let standardTupleCtor = FSharpValue.PreComputeTupleConstructor typeof<int * int * string>
    let fastTupleCtor = FSharpValue.PreComputeTupleConstructorFast typeof<int * int * string>
    
    let standardTupleReader = FSharpValue.PreComputeTupleReader typeof<int * int * string>
    let fastTupleReader = FSharpValue.PreComputeTupleReaderFast typeof<int * int * string>

    [<Test>]
    let ``should construct tuples faster than F# reflection`` () =
        let fast = repeat (fun i -> fastTupleCtor [|i ; i ; "s"|])
        let standard = repeat (fun i -> standardTupleCtor [|i ; i ; "s"|])
        printf "FSharpx is %ix faster" (standard/fast)
        Assert.True(fast < standard/ExpectedImprovement)

    [<Test>]
    let ``should read tuples faster than F# reflection`` () =
        let fast = repeat (fun i -> fastTupleReader (i,i,"s"))
        let standard = repeat (fun i -> standardTupleReader (i,i,"s"))
        printf "FSharpx is %ix faster" (standard/fast)
        Assert.True(fast < standard/ExpectedImprovement)
