namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("FSharpx.Reflection")>]
[<assembly: AssemblyProductAttribute("FSharpx.Reflection")>]
[<assembly: AssemblyDescriptionAttribute("Reflection helpers for F#")>]
[<assembly: AssemblyVersionAttribute("1.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0"
