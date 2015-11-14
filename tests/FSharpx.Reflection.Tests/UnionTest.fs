namespace FSharpx.Reflection.Tests

module UnionTest =

    open NUnit.Framework
    open Microsoft.FSharp.Reflection
    open System.Reflection //for bindingflags

    open FSharpx.Reflection

    type Straightforward =
        | Empty
        | S of string
        | I of int * string

    type Singleton = Single

    type internal Internal = internal | Si of string

    type internal Private = private | Fp of float

    type Generic<'a> = | Generic of 'a

    type LargeDU = A | B | C | E | F | H | Eye | J | K | L | M

    let straightforwardCases = FSharpType.GetUnionCases typeof<Straightforward>
    let singletonCase = (FSharpType.GetUnionCases typeof<Singleton>).[0]
    let internalCase = (FSharpType.GetUnionCases(typeof<Internal>, BindingFlags.NonPublic)).[0]
    let privateCase = (FSharpType.GetUnionCases(typeof<Private>, BindingFlags.NonPublic)).[0]
    let genericCase = (FSharpType.GetUnionCases typeof<Generic<int>>).[0]


    let getTagReaderComparer<'T> bindingFlags =
        let fastReader = FSharpValue.PreComputeUnionTagReaderFast(typeof<'T>, ?bindingFlags = bindingFlags)
        let reader = FSharpValue.PreComputeUnionTagReader(typeof<'T>, ?bindingFlags = bindingFlags)
        fun (x : 'T) -> Assert.AreEqual(reader x, fastReader x)

    [<Test>]
    let ``should construct Straightforward union case without arguments``() =
        let ctor = FSharpValue.PreComputeUnionConstructorFast straightforwardCases.[0]
        let resultObj = ctor [| |]
        let result = resultObj :?> Straightforward
        Assert.AreEqual(Empty, result)

    [<Test>]
    let ``should read Straightforward union case without arguments``() =
        let dtor = FSharpValue.PreComputeUnionReaderFast straightforwardCases.[0]
        let result = dtor Empty
        Assert.IsEmpty result

    [<Test>]
    let ``should construct Straightforward union case with one argument``() =
        let ctor = FSharpValue.PreComputeUnionConstructorFast straightforwardCases.[1]
        let resultObj = ctor [| "string" |]
        let result = resultObj :?> Straightforward
        Assert.AreEqual(S "string", result)

    [<Test>]
    let ``should read Straightforward union case with one argument``() =
        let dtor = FSharpValue.PreComputeUnionReaderFast straightforwardCases.[1]
        let result = dtor (S "string")
        Assert.AreEqual([| box "string" |], result)

    [<Test>]
    let ``should construct Straightforward union case with two arguments``() =
        let ctor = FSharpValue.PreComputeUnionConstructorFast straightforwardCases.[2]
        let resultObj = ctor [| 12; "string" |]
        let result = resultObj :?> Straightforward
        Assert.AreEqual(I (12,"string"), result)

    [<Test>]
    let ``should read Straightforward union case with two arguments``() =
        let dtor = FSharpValue.PreComputeUnionReaderFast straightforwardCases.[2]
        let result = dtor (I (12,"string"))
        Assert.AreEqual([| box 12; box "string" |], result)

    [<Test>]
    let ``should construct Singleton union case without arguments``() =
        let ctor = FSharpValue.PreComputeUnionConstructorFast singletonCase
        let resultObj = ctor [| |]
        let result = resultObj :?> Singleton
        Assert.AreEqual(Single, result)
        Assert.AreSame(Single, result) //singletons union cases are singletons

    [<Test>]
    let ``should read Singleton union case without arguments``() =
        let dtor = FSharpValue.PreComputeUnionReaderFast singletonCase
        let result = dtor Single
        Assert.IsEmpty result
    
    [<Test>]
    let ``should construct Internal union case``() =
        let ctor = FSharpValue.PreComputeUnionConstructorFast(internalCase, BindingFlags.NonPublic)
        let resultObj = ctor [| "string" |]
        let result = resultObj :?> Internal
        Assert.AreEqual(Si "string", result)

    [<Test>]
    let ``should read Internal union case``() =
        let dtor = FSharpValue.PreComputeUnionReaderFast(internalCase, BindingFlags.NonPublic)
        let result = dtor (Si "string")
        Assert.AreEqual([| box "string" |], result)

    [<Test>]
    let ``should not read Internal union case with BindingFlags.Public``() =
        Assert.Throws<System.ArgumentException>(new TestDelegate(fun () -> FSharpValue.PreComputeUnionReaderFast(internalCase) |> ignore))
        |> ignore

    [<Test>]
    let ``should construct Private union case``() =
        let ctor = FSharpValue.PreComputeUnionConstructorFast(privateCase, BindingFlags.NonPublic)
        let resultObj = ctor [| 1.23 |]
        let result = resultObj :?> Private
        Assert.AreEqual(Fp 1.23, result)

    [<Test>]
    let ``should read Private union case``() =
        let dtor = FSharpValue.PreComputeUnionReaderFast(privateCase, BindingFlags.NonPublic)
        let result = dtor (Fp 1.12)
        Assert.AreEqual([| box 1.12 |], result)

    [<Test>]
    let ``should construct Generic union case``() =
        let ctor = FSharpValue.PreComputeUnionConstructorFast genericCase
        let resultObj = ctor [| 2 |]
        let result = resultObj :?> Generic<int>
        Assert.AreEqual(Generic 2, result)

    [<Test>]
    let ``should read Generic union case``() =
        let dtor = FSharpValue.PreComputeUnionReaderFast genericCase
        let result = dtor (Generic 2)
        Assert.AreEqual([| box 2 |], result)
        

    [<Test>]
    let ``should read Straightforward union tag``() =
        let tester = getTagReaderComparer<Straightforward> None
        tester <| Empty
        tester <| S ""
        tester <| I(42,"")

    [<Test>]
    let ``should read Internal union tag`` () =
        let tester = getTagReaderComparer<Internal> (Some BindingFlags.NonPublic)
        tester <| Si ""

    [<Test>]
    let ``should read Private union tag`` () =
        let tester = getTagReaderComparer<Private> (Some BindingFlags.NonPublic)
        tester <| Fp 42.0

    [<Test>]
    let ``should read Generic union tag`` () =
        let tester = getTagReaderComparer<Generic<int * string>> None
        tester <| Generic (42,"")

    [<Test>]
    let ``should read Large union tag`` () =
        let tester = getTagReaderComparer<LargeDU> None
        tester A ; tester C ; tester H ; tester M

