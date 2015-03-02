module Program

open System
open System.Diagnostics
open Util

type Name = Env.Name
type Key = Env.Key
type Path<'I> = Env.Path<'I>


type Limit<'I> =
  | EqLimit of 'I
  | SpecLimit of 'I * ('I * 'I) list  

type Def<'I> = Def of Name * Limit<'I>

type Compiled = Compiled

type Domain = {
  name :Name;
  defs :Map<Name, Compiled>
}

exception CompilerFailure of string


let namedExternal : Def<Env.Path<Env.ScopeKey>> -> (Name * Env.ScopeKey) option =
  function
  | Def (name, EqLimit (path)) | Def (name, SpecLimit (path, _)) -> 
    match Env.findExternal path with
    | Some ext -> Some(name, ext)
    | None -> None


let bind : Env.ScopeKey -> Def<Path<Name>> list -> Def<Key> list =

  let externalizeDefs defs =
    let extLimit =
        function
        | EqLimit path -> EqLimit (Env.externalizePath path)
        | SpecLimit (path, args) ->
          SpecLimit (
            Env.externalizePath path,
            List.map (tupleApply Env.externalizePath) args)
    let extDef (Def(name, limit)) =
        Def (name, extLimit limit)
    in List.map extDef defs

  let bindKeys scope defs =
    let namedExts = Map.ofList (List.concat (List.map Option.toList (List.map namedExternal defs)))
    let keyOfPath = Env.keyOfPath scope (fun name -> Map.find name namedExts)
    let keyLimit =
        function
        | EqLimit path -> EqLimit (keyOfPath path)
        | SpecLimit (path, args) -> 
            let pathKey = keyOfPath path
            let keyOfParam = fun paramPath ->
                let paramName =
                  match paramPath with
                  | Env.LocalName name -> name
                  | _ -> raise (CompilerFailure (sprintf "In params position: %A" paramPath))
                match pathKey with
                | Env.Key (Env.LocalScopeKey(_), _) -> Env.Key (scope, paramName)
                | Env.Key (Env.ExternalScopeKey(_, _) as externalScope, _) -> Env.Key (externalScope, paramName)
            in SpecLimit (pathKey, List.map (fun (a, b) -> (keyOfParam a, keyOfPath b)) args)
    let keyDef (Def(name, limit)) =
        Def (name, keyLimit limit)
    in List.map keyDef defs

  fun scope defs -> bindKeys scope (externalizeDefs defs)

let scope : Option<Env.Scope<Def<Key>>> -> string -> Def<Path<Name>> list -> Env.Scope<Def<Key>> =
    fun parentScope scopeName defs ->
        let name = Env.scopeName parentScope + scopeName
        let scopeKey = Env.LocalScopeKey(name, Guid.NewGuid())
        let bound = bind scopeKey defs
        let content = List.map (function Def(name, c) as def-> (Env.Key(scopeKey,name), def)) bound
        in { Env.key = scopeKey; Env.parent = parentScope; Env.content = Map.ofList content}


// - well, refinement of existing roles (e.g. adding valueness or similar) is very much like 
//   creating new ones and limiting them by equality... 
// - and applying constraints in a speclimit is very much related as well...

// so.. what to do... well, we'll have to build a scope... and a scope key is available to us


[<EntryPoint>]
let main argv = 

    let defs : Def<Path<Name>> list = [
        Def ("a", EqLimit(Env.LocalName("eList")));
        Def ("eList", EqLimit(Env.ExternalAccess("List", "list")));
        Def ("eLength", EqLimit(Env.LocalAccess("eList", "length")));
        Def ("dList", SpecLimit(Env.ExternalAccess("List", "list"), [(Env.LocalName("element"),Env.LocalName("a"))]));
        Def ("dLength", EqLimit(Env.LocalAccess("dList", "length")))]
    let result = scope None "" defs
    Debug.Print (sprintf "Defs:\n%A" defs)
    Debug.Print (sprintf "Result:\n%A\n" result)
    0 
