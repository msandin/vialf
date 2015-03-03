module Program

open System
open System.Diagnostics
open Util

type Name = Scope.Name
type Key = Scope.Key
type Path<'I> = Scope.Path<'I>


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


let namedExternal : Def<Scope.Path<Scope.ScopeKey>> -> (Name * Scope.ScopeKey) option =
  function
  | Def (name, EqLimit (path)) | Def (name, SpecLimit (path, _)) -> 
    match Scope.findExternal path with
    | Some ext -> Some(name, ext)
    | None -> None


let bind : Scope.Scope option -> Name -> Def<Path<Name>> list -> Def<Key> list =

    fun parentScope scopeName defs -> 

        let scope = Scope.makeScope parentScope scopeName (List.map (function Def(name, _) -> name) defs)

        let externalizeDefs defs =
          let extLimit =
              function
              | EqLimit path -> EqLimit (Scope.externalizePath path)
              | SpecLimit (path, args) ->
                SpecLimit (
                  Scope.externalizePath path,
                  List.map (tupleApply Scope.externalizePath) args)
          let extDef (Def(name, limit)) =
              Def (name, extLimit limit)
          in List.map extDef defs

        let bindKeys defs =
            let namedExts = Map.ofList (List.concat (List.map Option.toList (List.map namedExternal defs)))
            let keyOfPath path =
                match Scope.keyOfPath scope (fun name -> Map.find name namedExts) path with
                | Some key -> key
                | None -> raise (CompilerFailure (sprintf "Not found: %A" path))
            let keyLimit =
                function
                | EqLimit path ->  EqLimit (keyOfPath path)
                | SpecLimit (path, args) -> 
                    let pathKey = keyOfPath path
                    let keyOfParam = fun paramPath ->
                        let paramName =
                            match paramPath with
                            | Scope.LocalName name -> name
                            | _ -> raise (CompilerFailure (sprintf "In params position: %A" paramPath))
                        match pathKey with
                        | Scope.Key (Scope.LocalScopeKey(_), _) -> Scope.Key (scope.key, paramName)
                        | Scope.Key (Scope.ExternalScopeKey(_, _) as externalScope, _) -> Scope.Key (externalScope, paramName)
                    in SpecLimit (pathKey, List.map (fun (a, b) -> (keyOfParam a, keyOfPath b)) args)
            let keyDef (Def(name, limit)) =
                Def (name, keyLimit limit)
            in List.map keyDef defs

        in bindKeys (externalizeDefs defs)

// - well, refinement of existing roles (e.g. adding valueness or similar) is very much like 
//   creating new ones and limiting them by equality... 
// - and applying constraints in a speclimit is very much related as well...

[<EntryPoint>]
let main argv = 

    let defs : Def<Path<Name>> list = [
        Def ("a", EqLimit(Scope.LocalName("eList")));
        Def ("eList", EqLimit(Scope.ExternalAccess("List", "list")));
        Def ("eLength", EqLimit(Scope.LocalAccess("eList", "length")));
        Def ("dList", SpecLimit(Scope.ExternalAccess("List", "list"), [(Scope.LocalName("element"),Scope.LocalName("a"))]));
        Def ("dLength", EqLimit(Scope.LocalAccess("dList", "length")))]
    let result = bind None "" defs
    Debug.Print (sprintf "Defs:\n%A" defs)
    Debug.Print (sprintf "Result:\n%A\n" result)
    0 
