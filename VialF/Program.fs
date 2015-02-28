module Program

open System
open System.Diagnostics
open Util

type Name = Env.Name
type Key = Env.Key
type Path<'I> = Env.Path<'I>


type Limit<'I> =
  | RefLimit of 'I
  | ApplyLimit of 'I * ('I * 'I) list  

type Def<'I> = Def of Name * Limit<'I>

type Compiled = Compiled

type Domain = {
  name :Name;
  defs :Map<Name, Compiled>
}

exception CompilerFailure of string


let namedExternal : Def<Env.Path<Env.Scope>> -> (Name * Env.Scope) option =
  function
  | Def (name, RefLimit (path)) | Def (name, ApplyLimit (path, _)) -> 
    match Env.findExternal path with
    | Some ext -> Some(name, ext)
    | None -> None


let bind : Env.Scope -> Def<Path<Name>> list -> Def<Key> list =

  let externalizeDefs defs =
    let extLimit =
        function
        | RefLimit path -> RefLimit (Env.externalizePath path)
        | ApplyLimit (path, args) ->
          ApplyLimit (
            Env.externalizePath path,
            List.map (tupleApply Env.externalizePath) args)
    let extDef (Def(name, limit)) =
        Def (name, extLimit limit)
    in List.map extDef defs

  let bindKeys scope defs =
    let namedExts = Map.ofList (List.concat (List.map optionList (List.map namedExternal defs)))
    let keyOfPath = Env.keyOfPath scope (fun name -> Map.find name namedExts)
    let keyLimit =
        function
        | RefLimit path -> RefLimit (keyOfPath path)
        | ApplyLimit (path, args) -> 
            let pathKey = keyOfPath path
            let keyOfParam = fun paramPath ->
                let paramName =
                  match paramPath with
                  | Env.LocalName name -> name
                  | _ -> raise (CompilerFailure (sprintf "In params position: %A" paramPath))
                match pathKey with
                | Env.Key (Env.LocalScope(_) , _) -> Env.Key (scope, paramName)
                | Env.Key (Env.ExternalScope(_, _) as externalScope, _) -> Env.Key (externalScope, paramName)
            in ApplyLimit (pathKey, List.map (fun (a, b) -> (keyOfParam a, keyOfPath b)) args)
    let keyDef (Def(name, limit)) =
        Def (name, keyLimit limit)
    in List.map keyDef defs

  fun scope defs -> bindKeys scope (externalizeDefs defs)

[<EntryPoint>]
let main argv = 

    let defs : Def<Path<Name>> list = [
        Def ("a", RefLimit(Env.LocalName("eList")));
        Def ("eList", RefLimit(Env.ExternalAccess("List", "list")));
        Def ("eLength", RefLimit(Env.LocalAccess("eList", "length")));
        Def ("dList", ApplyLimit(Env.ExternalAccess("List", "list"), [(Env.LocalName("element"),Env.LocalName("a"))]));
        Def ("dLength", RefLimit(Env.LocalAccess("dList", "length")))]
    let result = bind (Env.LocalScope ("/", Guid.NewGuid())) defs
    Debug.Print (sprintf "Defs:\n%A" defs)
    Debug.Print (sprintf "Result:\n%A\n" result)
    0 
