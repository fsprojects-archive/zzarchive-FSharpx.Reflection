namespace FSharpx.Reflection

//This uses some simple dynamic IL generation,
//and a clever technique desribed by Jon Skeet here:
//https://msmvps.com/blogs/jon_skeet/archive/2008/08/09/making-reflection-fly-and-exploring-delegates.aspx
module internal ReflectImpl =

    open System
    open System.Reflection
    open System.Reflection.Emit

    open Microsoft.FSharp.Reflection

    // simple typed descriptor for arguments/local vars in dynamic methods

    type EnvItem<'T>(ilGen : ILGenerator, ?argument : int16) = 
        inherit EnvItem(typeof<'T>, ilGen, ?argument = argument)

    and EnvItem(ty : Type, ilGen : ILGenerator, ?argument : int16) =

        let env = 
            match argument with
            | Some argId -> Arg argId
            | None -> LocalVar <| ilGen.DeclareLocal ty

        member __.Load () =
            match env with
            | Arg 0s -> ilGen.Emit OpCodes.Ldarg_0
            | Arg 1s -> ilGen.Emit OpCodes.Ldarg_1
            | Arg 2s -> ilGen.Emit OpCodes.Ldarg_2
            | Arg 3s -> ilGen.Emit OpCodes.Ldarg_3
            | Arg i -> ilGen.Emit (OpCodes.Ldarg_S, i)
            | LocalVar v -> ilGen.Emit(OpCodes.Ldloc, v)

        member __.Store () =
            match env with
            | LocalVar v -> ilGen.Emit(OpCodes.Stloc, v)
            | _ -> invalidOp "cannot store to arg param."

    and EnvDescriptor =
        | Arg of int16
        | LocalVar of LocalBuilder


    // wrappers for defining dynamic methods

    module DynamicMethod =

        type private Marker = class end

        let private createDynamicMethod (name : string) (argTypes : Type []) (returnType : Type) =
            let dyn =
                new DynamicMethod(name, 
                    MethodAttributes.Static ||| MethodAttributes.Public, CallingConventions.Standard, 
                    returnType, argTypes, typeof<Marker>, skipVisibility = true)

            dyn, dyn.GetILGenerator()

        let private compileDynamicMethod<'Dele when 'Dele :> Delegate> (dyn : DynamicMethod) =
            dyn.CreateDelegate(typeof<'Dele>) :?> 'Dele

        let compileFunc1<'U1,'V> (name : string) (builderF : EnvItem<'U1> -> ILGenerator -> unit) =
            let dyn, ilGen = createDynamicMethod name [| typeof<'U1> |] typeof<'V>
            let arg0 = EnvItem<'U1>(ilGen, 0s)
            do builderF arg0 ilGen
            compileDynamicMethod<Func<'U1,'V>> dyn

    let isGetterMethod (declaringType : Type) (m : MethodInfo) =
        not m.IsStatic 
            && not m.IsGenericMethod 
            && m.GetParameters().Length = 0
            && m.DeclaringType.IsAssignableFrom declaringType

    let wantNonPublic bindingFlags =
        let bindingFlags = defaultArg bindingFlags BindingFlags.Public
        bindingFlags &&& BindingFlags.NonPublic = BindingFlags.NonPublic

    // push an array of untyped parameters to the stack
    let pushParams offset (parameters : EnvItem<obj []>) (paramTypes : Type []) (ilGen : ILGenerator) =
        let pushParam (i : int) (ty : Type) =
            parameters.Load ()
            ilGen.Emit(OpCodes.Ldc_I4, i)
            ilGen.Emit OpCodes.Ldelem_Ref
            if ty <> typeof<obj> then
                if ty.IsValueType then
                    ilGen.Emit(OpCodes.Unbox_Any, ty)
                else
                    ilGen.Emit(OpCodes.Castclass, ty)

        paramTypes |> Array.iteri (fun i t -> pushParam (offset + i) t)

    // evaluate a collection of getters and store to a given array
    let saveParams offset (parameters : EnvItem<obj []>) (self : EnvItem) (getters : MethodInfo []) (ilGen : ILGenerator) =
        let saveParam (idx : int) (getter : MethodInfo) =
            // arr.[idx] <- p.GetValue(o) :> obj
            parameters.Load ()
            ilGen.Emit(OpCodes.Ldc_I4, idx)

            // call property getter
            self.Load()
            ilGen.EmitCall(OpCodes.Call, getter, null)
            if getter.ReturnType.IsValueType then ilGen.Emit(OpCodes.Box, getter.ReturnType)

            // store
            ilGen.Emit OpCodes.Stelem_Ref

        getters |> Array.iteri (fun i g -> saveParam (offset + i) g)

    let preComputeConstructor (ctorInfo : ConstructorInfo) =
        DynamicMethod.compileFunc1<obj[], obj> "untypedCtor" (fun cparams ilGen ->
            let paramTypes = ctorInfo.GetParameters() |> Array.map (fun pi -> pi.ParameterType)

            pushParams 0 cparams paramTypes ilGen
            ilGen.Emit(OpCodes.Newobj, ctorInfo)
            if ctorInfo.DeclaringType.IsValueType then ilGen.Emit(OpCodes.Box, ctorInfo.DeclaringType)
            ilGen.Emit OpCodes.Ret)

    // bundles multiple property getters in one dynamic method
    let preComputeGetterMethods (declaringType : Type) (getters : MethodInfo []) =
        assert (getters |> Array.forall (isGetterMethod declaringType))
        
        if getters.Length = 0 then None else

        DynamicMethod.compileFunc1<obj, obj []> "untypedGetters" (fun self ilGen ->

            // local declarations
            let unboxed = EnvItem(declaringType, ilGen)
            let arr = EnvItem<obj []>(ilGen)

            // unbox input
            self.Load ()
            ilGen.Emit(OpCodes.Unbox_Any, declaringType)
            unboxed.Store()

            // init obj array
            ilGen.Emit(OpCodes.Ldc_I4, getters.Length)
            ilGen.Emit(OpCodes.Newarr, typeof<obj>)
            arr.Store ()

            // evaluate getters and store to array
            saveParams 0 arr unboxed getters ilGen

            // return array
            arr.Load ()
            ilGen.Emit OpCodes.Ret) |> Some

    let preComputePropertyGetters bindingFlags (declaringType : Type) (props : PropertyInfo []) =
        let getMethods = 
            props |> Array.map (fun p ->
                match p.GetGetMethod(wantNonPublic bindingFlags) with
                | null ->
                    sprintf "The type '%s' has private representation. You must specify BindingFlags.NonPublic to access private type representations." declaringType.Name
                    |> invalidArg "bindingFlags"
                | p -> p)

        preComputeGetterMethods declaringType getMethods


    // F# type constructors
    
    let preComputeRecordContructor(recordType:Type,bindingFlags:BindingFlags option) =
        assert FSharpType.IsRecord(recordType, ?bindingFlags=bindingFlags)
        let ctorInfo = FSharpValue.PreComputeRecordConstructorInfo(recordType,?bindingFlags=bindingFlags)
        preComputeConstructor ctorInfo
    
    let preComputeUnionConstructor(unionCaseInfo:UnionCaseInfo, bindingFlags:BindingFlags option) =
        let methodInfo = FSharpValue.PreComputeUnionConstructorInfo(unionCaseInfo, ?bindingFlags=bindingFlags)
        let targetType = methodInfo.DeclaringType
        assert FSharpType.IsUnion(targetType, ?bindingFlags=bindingFlags)
        let paramTypes = methodInfo.GetParameters() |> Array.map (fun pi -> pi.ParameterType)

        DynamicMethod.compileFunc1<obj [], obj> "unionCtor" (fun cparams ilGen ->
            pushParams 0 cparams paramTypes ilGen
            ilGen.Emit(OpCodes.Call, methodInfo)
            ilGen.Emit OpCodes.Ret)

    let preComputeTupleConstructor(tuple : Type) =
        DynamicMethod.compileFunc1<obj [], obj> "tupleCtor" (fun cparams ilGen ->
            // walk through potentially nested tuples
            let rec traverse offset (tuple : Type) =
                let ctorInfo, nested = FSharpValue.PreComputeTupleConstructorInfo tuple
                let paramTypes = ctorInfo.GetParameters() |> Array.map (fun pi -> pi.ParameterType)

                match nested with
                | None -> pushParams offset cparams paramTypes ilGen
                | Some nested ->
                    let n = paramTypes.Length
                    pushParams offset cparams paramTypes.[..n-2] ilGen
                    traverse (offset + n - 1) nested

                ilGen.Emit(OpCodes.Newobj, ctorInfo)

            traverse 0 tuple
            ilGen.Emit OpCodes.Ret)

    // an implementation that curiously does not exist in Microsoft.FSharp.Reflection
    let preComputeExceptionConstructorInfo(exceptionType : Type, bindingFlags:BindingFlags option) : ConstructorInfo =
        assert FSharpType.IsExceptionRepresentation(exceptionType, ?bindingFlags = bindingFlags)
        let signature = FSharpType.GetExceptionFields(exceptionType, ?bindingFlags = bindingFlags) |> Array.map(fun f -> f.PropertyType)
        let ctors = 
            match bindingFlags with 
            | Some f -> exceptionType.GetConstructors (f ||| BindingFlags.Instance) 
            | None -> exceptionType.GetConstructors()

        match ctors |> Array.tryFind(fun ctor -> signature = (ctor.GetParameters() |> Array.map(fun p -> p.ParameterType))) with
        | None -> invalidArg "exnType" "The exception type is private. You must specify BindingFlags.NonPublic to access private type representations."
        | Some ctorInfo -> ctorInfo

    let preComputeExceptionConstructor(exceptionType : Type, bindingFlags:BindingFlags option) =
        preComputeExceptionConstructorInfo(exceptionType, bindingFlags) |> preComputeConstructor


    // F# type readers

    let preComputeRecordReader (recordType:Type, bindingFlags:BindingFlags option) =
        let fields = FSharpType.GetRecordFields(recordType, ?bindingFlags=bindingFlags)
        preComputePropertyGetters bindingFlags recordType fields

    let preComputeUnionReader(unionCase:UnionCaseInfo, bindingFlags:BindingFlags option) =
        let fields = unionCase.GetFields()
        let declaringType = if fields.Length = 0 then unionCase.DeclaringType else fields.[0].DeclaringType
        preComputePropertyGetters bindingFlags declaringType fields

    let preComputeTupleReader (tuple : Type) =
        let size = FSharpType.GetTupleElements tuple |> Array.length
        DynamicMethod.compileFunc1<obj, obj []> "tupleReader" (fun self ilGen ->

            // local vars
            let arr = EnvItem<obj []>(ilGen)
            let currentTuple = EnvItem<obj>(ilGen)

            // init obj array
            ilGen.Emit(OpCodes.Ldc_I4, size)
            ilGen.Emit(OpCodes.Newarr, typeof<obj>)
            arr.Store()

            // store input
            self.Load()
            currentTuple.Store()

            let rec traverse offset (tuple : Type) =
                let fields = 
                    tuple.GetProperties() 
                    |> Seq.filter (fun p -> p.Name.StartsWith("Item")) 
                    |> Seq.sortBy (fun p -> p.Name) //need: Items < 10
                    |> Seq.map (fun p -> p.GetGetMethod(true))
                    |> Seq.toArray

                saveParams offset arr currentTuple fields ilGen

                match tuple.GetProperty("Rest") with
                | null -> ()
                | rest ->
                    let rest = rest.GetGetMethod(true)
                    // replace with current tuple
                    currentTuple.Load()
                    ilGen.Emit(OpCodes.Call, rest)
                    currentTuple.Store()
                    traverse (offset + fields.Length) rest.ReturnType

            do traverse 0 tuple
            arr.Load()
            ilGen.Emit OpCodes.Ret)

    let preComputeExceptionReader(exnT : Type, bindingFlags:BindingFlags option) =
        let fields = FSharpType.GetExceptionFields(exnT, ?bindingFlags = bindingFlags)
        preComputePropertyGetters bindingFlags exnT fields


    // fast union tag reader
    let preComputeUnionTagReader(union : Type, bindingFlags) =
        let tagGetter =
            match FSharpValue.PreComputeUnionTagMemberInfo(union, ?bindingFlags = bindingFlags) with
            | null -> invalidArg "bindingFlags" "The union type is private. You must specify BindingFlags.NonPublic to access private type representations."
            | :? PropertyInfo as p -> p.GetGetMethod(true)
            | :? MethodInfo as m -> m
            | _ -> invalidOp "unexpected error"

        DynamicMethod.compileFunc1<obj, int> "unionTagReader" (fun union ilGen ->
            union.Load()
            ilGen.Emit(OpCodes.Call, tagGetter)
            ilGen.Emit OpCodes.Ret)


    let inline ofFunc(f : Func<'T, 'S>) : 'T -> 'S = f.Invoke

    let inline ofOptionalFunc(f : Func<'T, 'S []> option) : 'T -> 'S [] =
        match f with
        | None -> fun _ -> [||]
        | Some f -> f.Invoke
