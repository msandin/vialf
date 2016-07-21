open Util
open System.IO
open System.Reflection
open FParsec

module Example = 
    open Vial.Eval
    let print =
        function
        | [(_, Text text)] -> Debug.println text; Nothing
        | [(_, e)] -> Debug.println (sprintf "EXPRESSION:\n%A" e); Nothing
        | args -> failwith "Bad arguments: %A" args

    let outerEnv = 
        bindings [external "print" print]

    //open Vial.Syntax
//    open Vial.Interpret
//
//    let print =
//        function
//        | [(_, Text text)] -> Debug.println text; Nothing
//        | [(_, e)] -> Debug.println (sprintf "%A" e); Nothing
//        | args -> failwith "Bad arguments: %A" args
//
//    let outerEnv = 
//        Env.bindings [Env.external "print" print]

//    let block =
//        {declarations=
//            [def "a" (Some(Text "A!"));
//             def "b" (Some(Text "B!"));
//             def "main" (Some(Dispatch (path ["print"], [("text", access ["a"])])))]}
//
//    let program = dispatch ["print"] [arg "text" (access ["X"; "c"])]


let load () = 
    let srcFile = 
        Path.GetFullPath
            (Path.Combine
                 (Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "..", "..", "examples", "example1.vial"))
    let src = File.ReadAllText(srcFile)
    Vial.Parser.parse src    

[<EntryPoint>]
let main argv =
    let ast = load ()
    in match ast with
        | Failure(errorMsg, _, _) -> Debug.println (sprintf "Failure: %s" errorMsg)
        | Success(blocks, _, _) ->
            let evalEnv env ((name, path), block) =
                Debug.println (sprintf "Block: %s %A %A" name path block)
                let env = Vial.Eval.bindContext env name path block
                match Vial.Eval.tryLookup env (Vial.Syntax.path [name; "main"]) with
                | None -> Debug.println (sprintf "No main found in %s." name)
                | Some value ->
                    let result = Vial.Eval.call value []
                    Debug.println (sprintf "Result of %s.main: %A" name result)
                env
            ignore (List.fold evalEnv (Example.outerEnv) blocks)
    0
//            let evalEnv env ((name, path), block) = 
//                Debug.println (sprintf "Block: %s %A %A" name path block)
//                let parentEnv = (Option.map (fun p -> Vial.Interpret.Env.accessPath env p)) path
//                let blockEnv = Vial.Interpret.buildBlockEnv env (block) // so, now how do we construct the new with parent? 
//                match Vial.Interpret.Env.tryAccess blockEnv "main" with
//                | None -> Debug.println (sprintf "No main found in %s." name)
//                | Some (x) ->                    
//                    match x.value with
//                    | Some (value) ->
//                        let result = Vial.Interpret.evalExpr blockEnv value []
//                        Debug.println (sprintf "Result of %s.main: %A" name result)
//                    | _ -> Debug.println "Non-expr main found."
//                Vial.Interpret.Env.bindContext env name blockEnv
//            ignore (List.fold evalEnv (Example.outerEnv) blocks)  




//0 // return an integer exit code