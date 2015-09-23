module Program

open System
open System.IO
open System.Reflection
open System.Diagnostics
open Util
open FParsec


// next things to do is to
// -) print
// -) create a main program which loads a bunch of files and resolves deps?
// -) resolve namespaces...

[<EntryPoint>]
let main argv =
    let srcFile = 
        Path.GetFullPath
            (Path.Combine
                 (Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "..", "..", "examples", "example1.vial"))
    let src = File.ReadAllText(srcFile)
    let ast = Syntax.parse src
    in match ast with
        | Failure(errorMsg, _, _) -> Debug.Print (sprintf "Failure: %s" errorMsg)
        | Success(source, _, _) ->
            Debug.Print (sprintf "Source: %A" source)
            let env = Scope.empty
            let loc = Model.Location.rootLocation source.space
            let scope = Model.Domains.bind loc env source.domains 
            Debug.Print (sprintf "Domains: %A" (Scope.extract scope))
    0
