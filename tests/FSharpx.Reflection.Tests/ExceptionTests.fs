namespace FSharpx.Reflection.Tests

module ExceptionTest =

    open System.Reflection
    open NUnit.Framework
    open FSharpx.Reflection

    exception SimpleException

    exception PublicException of string * obj

    exception private PrivateException of string * int

    [<Test>]
    let ``should construct simple exception`` () =
        let ctor = FSharpValue.PreComputeExceptionConstructorFast typeof<SimpleException>
        Assert.AreEqual(SimpleException, ctor [||])

    [<Test>]
    let ``should construct public exception`` () =
       let ctor = FSharpValue.PreComputeExceptionConstructorFast typeof<PublicException> 
       Assert.AreEqual(PublicException("", box 42), ctor [| box "" ; box 42 |])

    [<Test>]
    let ``should construct private exception`` () =
        let ctor = FSharpValue.PreComputeExceptionConstructorFast(typeof<PrivateException>, BindingFlags.NonPublic)
        Assert.AreEqual(PrivateException("", 42), ctor [| box "" ; box 42 |])

    [<Test>]
    let ``should read simple exception`` () =
        let dtor = FSharpValue.PreComputeExceptionReaderFast typeof<SimpleException>
        Assert.AreEqual([||], dtor SimpleException)

    [<Test>]
    let ``should read pubic exception`` () =
        let dtor = FSharpValue.PreComputeExceptionReaderFast typeof<PublicException>
        Assert.AreEqual([|box "" ; box 42|], dtor <| PublicException("", box 42))

    [<Test>]
    let ``should read private exception`` () =
        let dtor = FSharpValue.PreComputeExceptionReaderFast(typeof<PrivateException>, BindingFlags.NonPublic)
        Assert.AreEqual([|box ""; box 42|], dtor <| PrivateException("", 42))

