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


type Expr<'I> =
  | RefExpr of 'I
  | FunExpr of ('I * 'I) list * Expr<'I>

type Def<'I> = Def of Name * Limit<'I> option * Expr<'I> option

type Compiled = Compiled

type Domain = {
  name :Name;
  defs :Map<Name, Compiled>
}

exception CompilerFailure of string


let namedExternal : Def<Scope.Path<Scope.ScopeKey>> -> (Name * Scope.ScopeKey) option =
  function
  | Def (name, Some(EqLimit (path)), _) | Def (name, Some(SpecLimit (path, _)), _) -> 
    match Scope.findExternal path with
    | Some ext -> Some(name, ext)
    | None -> None
  | _ -> None

let bind : Scope.Scope option -> Name -> Def<Path<Name>> list -> Def<Key> list =

    fun parentScope scopeName defs -> 

        let scope = Scope.makeScope parentScope scopeName (List.map (function Def(name, _, _) -> name) defs)

        let externalizeDefs defs = 
            let extLimit = 
                function 
                | EqLimit path -> EqLimit(Scope.externalizePath path)
                | SpecLimit(path, args) -> 
                    SpecLimit(Scope.externalizePath path, List.map (tupleApply Scope.externalizePath) args)
            
            let rec extExpr = 
                function 
                | RefExpr path -> RefExpr(Scope.externalizePath path)
                | FunExpr(parameters, body) -> FunExpr(List.map (tupleApply Scope.externalizePath) parameters, extExpr body)
            
            let extDef (Def(name, limit, expr)) = Def(name, Option.map extLimit limit, Option.map extExpr expr)
            List.map extDef defs

        let bindKeys defs =
            let keyOfPath path =
                let namedExts = Map.ofList (List.concat (List.map Option.toList (List.map namedExternal defs)))
                match Scope.keyOfPath scope (fun name -> Map.find name namedExts) path with
                | Some key -> key
                | None -> raise (CompilerFailure (sprintf "Not found: %A" path))
            let keyLimit =
                function
                | EqLimit path -> EqLimit (keyOfPath path)
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
            let rec keyExpr = 
                function
                | RefExpr path -> RefExpr (keyOfPath path)
                | FunExpr (parameters, body) ->
                    FunExpr ([], keyExpr body)
                        // <-- but... we do want to introduce a new scope for this
                        // <-- honestly, we should probably recurse for this
            let keyDef (Def(name, limit, expr)) =
                Def (name, Option.map keyLimit limit, None) // <-- None at this time, will use keyExpr
            in List.map keyDef defs

        in bindKeys (externalizeDefs defs)

// - well, refinement of existing roles (e.g. adding valueness or similar) is very much like 
//   creating new ones and limiting them by equality... 
// - and applying constraints in a speclimit is very much related as well...

[<EntryPoint>]
let main argv = 

    let defs : Def<Path<Name>> list = [
        Def ("a", Some (EqLimit(Scope.LocalName("eList"))), None);
        Def ("eList", Some (EqLimit(Scope.ExternalAccess("List", "list"))), None);
        Def ("eLength", Some (EqLimit(Scope.LocalAccess("eList", "length"))), None);
        Def ("dList", Some (SpecLimit(Scope.ExternalAccess("List", "list"), [(Scope.LocalName("element"),Scope.LocalName("a"))])), None);
        Def ("dLength", Some (EqLimit(Scope.LocalAccess("dList", "length"))), None)]
    let result = bind None "" defs
    Debug.Print (sprintf "Defs:\n%A" defs)
    Debug.Print (sprintf "Result:\n%A\n" result)
    0 
