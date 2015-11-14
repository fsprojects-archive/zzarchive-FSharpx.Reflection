namespace FSharpx.Reflection

open System
open System.Reflection
open Microsoft.FSharp.Reflection

///Contains operations associated with constructing and analyzing values 
///associated with F# types such as records, unions and tuples.
type FSharpValue =
    ///Generates a function for constructing a record value.
    static member PreComputeRecordConstructorFast(recordType:Type,?bindingFlags:BindingFlags) =
        ReflectImpl.preComputeRecordContructor(recordType,bindingFlags) 
        |> ReflectImpl.ofFunc

    ///Generates a function for constructing a union value.
    static member PreComputeUnionConstructorFast(unionCase:UnionCaseInfo, ?bindingFlags:BindingFlags) =
        ReflectImpl.preComputeUnionConstructor(unionCase,bindingFlags) 
        |> ReflectImpl.ofFunc

    ///Generates a function for constructing a function value.
    static member PreComputeTupleConstructorFast(tupleType:Type) =
        ReflectImpl.preComputeTupleConstructor tupleType 
        |> ReflectImpl.ofFunc

    ///Generates a function for constructing an exception.
    static member PreComputeExceptionConstructorFast(exceptionType:Type,?bindingFlags) =
        ReflectImpl.preComputeExceptionConstructor(exceptionType,bindingFlags) 
        |> ReflectImpl.ofFunc

    ///Precompute a function for reading all the fields from a record.
    static member PreComputeRecordReaderFast(recordType:Type, ?bindingFlags:BindingFlags) : obj -> obj[] =
        ReflectImpl.preComputeRecordReader(recordType,bindingFlags) 
        |> ReflectImpl.ofOptionalFunc

    ///Precompute a function for reading all the fields from a union case.
    static member PreComputeUnionReaderFast(unionCase:UnionCaseInfo, ?bindingFlags:BindingFlags) : obj -> obj[] =
        ReflectImpl.preComputeUnionReader(unionCase, bindingFlags) 
        |> ReflectImpl.ofOptionalFunc

    ///Precompute a function for reading all the fields from a tuple.
    static member PreComputeTupleReaderFast(tupleType:Type) : obj -> obj [] =
        ReflectImpl.preComputeTupleReader tupleType 
        |> ReflectImpl.ofFunc

    ///Precompute a function for reading all the fields from an exception.
    static member PreComputeExceptionReaderFast(exceptionType:Type,?bindingFlags) : obj -> obj [] =
        ReflectImpl.preComputeExceptionReader(exceptionType,bindingFlags) 
        |> ReflectImpl.ofOptionalFunc

    ///Precompute the ConstructorInfo of the given exception type.
    static member PreComputeExceptionConstructorInfo(exceptionType,?bindingFlags) : ConstructorInfo =
        ReflectImpl.preComputeExceptionConstructorInfo(exceptionType,bindingFlags)

    ///Generates a function to read the tags of a union type.
    static member PreComputeUnionTagReaderFast(unionType:Type,?bindingFlags:BindingFlags) : obj -> int =
        ReflectImpl.preComputeUnionTagReader(unionType,bindingFlags) 
        |> ReflectImpl.ofFunc

