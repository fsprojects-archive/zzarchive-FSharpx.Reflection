namespace FSharpx.Reflection.Tests
  
module RecordTest =

    open NUnit.Framework
    open System.Reflection //for bindingflags
    open FSharpx.Reflection
    
    //some test types

    type Straightforward =
            { S : string
              I : int
              F : float
              O : obj } //object is somewhat special cased the constructor code

    type Mutable =
        { mutable Sm : string
          mutable Im : int
          mutable Fm : float }

    type internal Internal = internal { Si : string }

    type private Private = private { Fp : float }

    type Generic<'a> = { Generic : 'a }

    [<Test>]
    let ``should construct Straightforward record``() =
        let ctor = FSharpValue.PreComputeRecordConstructorFast(typeof<Straightforward>)
        let data = [| box "string"; box 12; box 15.12; new obj() |]

        let result = ctor data
        Assert.IsInstanceOf(typeof<Straightforward>, result)
        let primResult = result :?> Straightforward
        Assert.AreEqual("string", primResult.S)
        Assert.AreEqual(12, primResult.I)
        Assert.AreEqual(15.12, primResult.F)
        Assert.AreEqual(data.[3], primResult.O)


    [<Test>]
    let ``should read Straightforward record``() =
        let dtor = FSharpValue.PreComputeRecordReaderFast typeof<Straightforward>
        let data = { S = "string"; I = 12; F = 15.12; O = new obj() }

        let result = dtor data
        Assert.AreEqual(data.S, unbox<string> result.[0])
        Assert.AreEqual(data.I, unbox<int> result.[1])
        Assert.AreEqual(data.F, unbox<float> result.[2])
        Assert.AreSame(data.O, result.[3])

    [<Test>]
    let ``should construct Mutable record``() =
        let ctor = FSharpValue.PreComputeRecordConstructorFast(typeof<Mutable>)
        let data = [| box "string"; box 12; box 15.12 |]

        let result = ctor data
        let primResult = result :?> Mutable
        Assert.AreEqual("string", primResult.Sm)
        Assert.AreEqual(12, primResult.Im)
        Assert.AreEqual(15.12, primResult.Fm)


    [<Test>]
    let ``should read Mutable record``() =
        let dtor = FSharpValue.PreComputeRecordReaderFast typeof<Mutable>
        let data = { Sm = "string"; Im = 12; Fm = 15.12 }

        let result = dtor data
        Assert.AreEqual(data.Sm, unbox<string> result.[0])
        Assert.AreEqual(data.Im, unbox<int> result.[1])
        Assert.AreEqual(data.Fm, unbox<float> result.[2])

    [<Test>]
    let ``should construct Internal record``() =
        let ctor = FSharpValue.PreComputeRecordConstructorFast(typeof<Internal>, BindingFlags.NonPublic)
        let data = [| box "string" |]

        let result = ctor data
        let primResult = result :?> Internal
        Assert.AreEqual("string", primResult.Si)


    [<Test>]
    let ``should read Internal record``() =
        let dtor = FSharpValue.PreComputeRecordReaderFast(typeof<Internal>, BindingFlags.NonPublic)
        let data = { Si = "string" }

        let result = dtor data
        Assert.AreEqual(data.Si, unbox<string> result.[0])

    [<Test>]
    let ``should construct Private record``() =
        let ctor = FSharpValue.PreComputeRecordConstructorFast(typeof<Private>, BindingFlags.NonPublic)
        let data = [| box 1.2 |]

        let result = ctor data
        let primResult = result :?> Private
        Assert.AreEqual(1.2, primResult.Fp)


    [<Test>]
    let ``should read Private record``() =
        let dtor = FSharpValue.PreComputeRecordReaderFast(typeof<Private>, BindingFlags.NonPublic)
        let data = { Fp = 2.1 }

        let result = dtor data
        Assert.AreEqual(data.Fp, unbox<float> result.[0])

    [<Test>]
    let ``should construct Generic record``() =
        let ctor = FSharpValue.PreComputeRecordConstructorFast(typeof<Generic<int>>)
        let data = [| box 1 |]

        let result = ctor data :?> Generic<int>
        Assert.AreEqual(1, result.Generic)


    [<Test>]
    let ``should read Generic record``() =
        let dtor = FSharpValue.PreComputeRecordReaderFast(typeof<Generic<int>>)
        let data = { Generic = 2 }

        let result = dtor data
        Assert.AreEqual(data.Generic, unbox<int> result.[0])
        







