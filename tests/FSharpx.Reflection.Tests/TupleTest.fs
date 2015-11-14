namespace FSharpx.Reflection.Tests

module TupleTest =

    open System
    open NUnit.Framework
    open FSharpx.Reflection
    
    [<Test>]
    let ``should construct unitary tuple`` () =
        let ctor = FSharpValue.PreComputeTupleConstructorFast typeof<Tuple<int>>
        Assert.AreEqual(Tuple<_>(42), ctor [| box 42 |])

    [<Test>]
    let ``should read unitary tuple`` () =
        let dtor = FSharpValue.PreComputeTupleReaderFast typeof<Tuple<int>>
        Assert.AreEqual([| box 42 |], dtor <| Tuple<_>(42))

    [<Test>]
    let ``should construct pair`` () =
        let ctor = FSharpValue.PreComputeTupleConstructorFast typeof<int * string>
        Assert.AreEqual((42,""), ctor [| box 42 ; "" |])

    [<Test>]
    let ``should read pair`` () =
        let dtor = FSharpValue.PreComputeTupleReaderFast typeof<int * string>
        Assert.AreEqual([| box 42 ; box "" |], dtor (42, ""))

    [<Test>]
    let ``should construct large tuple`` () =
        let ctor = FSharpValue.PreComputeTupleConstructorFast typeof<int * string * bool * (int * string) * int * int * int * int * int>
        Assert.AreEqual((42, "", false, (10, " "),1 ,2 ,3 ,4 ,5), ctor [| box 42; "" ; false ; (10," ") ; 1 ; 2 ; 3 ; 4 ; 5 |])

    [<Test>]
    let ``should read large tuple`` () =
        let dtor = FSharpValue.PreComputeTupleReaderFast typeof<int * string * bool * (int * string) * int * int * int * int * int>
        Assert.AreEqual(([| 42; "" ; false ; (10," ") ; 1 ; 2 ; 3 ; 4 ; 5 |] : obj []), dtor (42, "", false, (10, " "), 1, 2, 3, 4, 5))

