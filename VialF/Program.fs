module Program

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


let namedExternal : Def<Env.Path<Env.External>> -> (Name * Env.External) option =
  function
  | Def (name, RefLimit (path)) -> 
    match Env.findExternal path with
    | Some ext -> Some(name, ext)
    | None -> None 
  | _ -> None


let bind : Def<Path<Name>> list -> Def<Key> list =

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

  let bindKeys defs =
    let namedExts = Map.ofList (List.concat (List.map optionList (List.map namedExternal defs)))
    let keyOfPath = Env.keyOfPath (fun name -> Map.find name namedExts)
    let keyLimit =
        function
        | RefLimit path -> RefLimit (keyOfPath path)
        | ApplyLimit (path, args) -> 
            let pathKey = keyOfPath path
            // now, we need to build
            let keyOfParam = fun paramPath ->
                let paramName =
                  match paramPath with
                  | Env.LocalName name -> name
                  | _ -> raise (CompilerFailure (sprintf "In params position: %A" paramPath))
                match pathKey with
                | Env.LocalKey name -> Env.LocalKey paramName
                | Env.ExternalKey (external, name) -> Env.ExternalKey (external, paramName)
            in ApplyLimit (pathKey, List.map (fun (a, b) -> (keyOfParam a, keyOfPath b)) args)
    let keyDef (Def(name, limit)) =
        Def (name, keyLimit limit)
    in List.map keyDef defs

  fun defs -> bindKeys (externalizeDefs defs)

// the next problem then is applylimits, e.g. we should be able to add stuff to exts so that it knows that it's the original + limits
// but that's not really the purpose of external... is it? the external is more of the unique key for doing that,
// what we need to do is rather to invent some sort of concept of environments... I'd say... we'll have to deal with more kinds
// of limits and whatnot as well but... one thing at a time I say.

// first we should change external into scope and do ExternalScope and LocalScope... this way the key is always a scope and a 
// name... so should we pass in the local scope and build the scope keys as a hieararchy? maybe... well... we will have to pass in
// the global scope... that might make some of the code reusable recursively... interesting...

[<EntryPoint>]
let main argv = 

    let defs : Def<Path<Name>> list = [
        Def ("a", RefLimit(Env.LocalName("eList")));
        Def ("eList", RefLimit(Env.ExternalAccess("List", "list")));
        Def ("eLength", RefLimit(Env.LocalAccess("eList", "length")));
        Def ("dList", ApplyLimit(Env.ExternalAccess("List", "list"), [(Env.LocalName("element"),Env.LocalName("a"))]))]
    let result = bind defs
    Debug.Print (sprintf "Defs:\n%A" defs)
    Debug.Print (sprintf "Result:\n%A\n" result)
    0 
