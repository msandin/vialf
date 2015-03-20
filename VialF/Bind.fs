module Bind

open System
open System.Diagnostics
open Util


exception BindFailure of string 

type Name = Scope.Name
type Path<'I> = Scope.Path<'I>
type Key = Scope.Key

let nameOfPath =
    function
    | Scope.LocalName name -> name
    | path -> raise (BindFailure (sprintf "In params position: %A" path))

// externals
let namedExternal : Model.Def<Scope.Path<Scope.ScopeKey>> -> (Name * Scope.ScopeKey) option = 
    function 
    | Model.Def(defPath, _, Some(Model.EqLimit(path)), _) | Model.Def(defPath, _, Some(Model.SpecLimit(path, _)), _) -> 
        match Scope.findExternal path with
        | Some ext -> Some(nameOfPath defPath, ext)
        | None -> None
    | _ -> None

let rec extPath : Path<Name> -> Path<Scope.ScopeKey> =
    function
    | Scope.LocalName name -> Scope.LocalName name
    | Scope.LocalAccess (name, sub) -> Scope.LocalAccess (name, sub)
    | Scope.ExternalAccess (extName, name) -> Scope.ExternalAccess (Scope.ExternalScopeKey (extName, Guid.NewGuid ()), name)

let rec extLimit = 
    function 
    | Model.EqLimit path -> Model.EqLimit(extPath path)
    | Model.SpecLimit(path, args) -> 
        Model.SpecLimit(
            extPath path,
            List.map (function Model.Arg(a, b) -> Model.Arg (extPath a, extLimit b)) args)

let rec extExpr = 
    function 
    | Model.RefExpr path -> Model.RefExpr(extPath path)
    | Model.FunExpr (parameters, body) ->
        Model.FunExpr(
            List.map (function (Model.Param (path, _, limit)) -> Model.Param (extPath path, None, Option.map extLimit limit)) parameters,
            extExpr body)
    | Model.ApplyExpr (body, args) -> Model.ApplyExpr (extPath body, List.map (function Model.ApplyArg (path, body) -> Model.ApplyArg(extPath path, extExpr body)) args)

let extDefs =            
    let extDef (Model.Def(path, _, limit, expr)) =
        Model.Def(extPath path, None, Option.map extLimit limit, Option.map extExpr expr)
    List.map extDef 

// bind
let bindPath scope lookupExt path =
    match Scope.keyOfPath scope lookupExt path with
    | Some key -> key
    | None -> raise (BindFailure (sprintf "Not found: %A" path))

let rec bindLimit scope lookupExt =
    let keyOfPath = bindPath scope lookupExt
    function
    | Model.EqLimit path -> Model.EqLimit (keyOfPath path)
    | Model.SpecLimit (path, args) -> 
        let pathKey = keyOfPath path
        let keyOfParam = fun paramPath ->
            let paramName = nameOfPath paramPath
            match pathKey with
            | Scope.Key (Scope.LocalScopeKey(_), _) -> Scope.Key (scope.key, paramName)
            | Scope.Key (Scope.ExternalScopeKey(_, _) as externalScopeKey, _) -> Scope.Key (externalScopeKey, paramName)
        in Model.SpecLimit (pathKey, List.map (function Model.Arg (a, b) -> Model.Arg (keyOfParam a, bindLimit scope lookupExt b)) args)

let rec bindExpr name scope lookupExt = 
    function
    | Model.RefExpr path -> Model.RefExpr (bindPath scope lookupExt path)
    | Model.FunExpr (parameters, body) ->
        let names = List.map (function Model.Param(path, _, limit) -> nameOfPath path) parameters
        let innerScope = Scope.makeScope (Some scope) name names
        in Model.FunExpr (bindParams innerScope lookupExt parameters, bindExpr name innerScope lookupExt body)
    | Model.ApplyExpr (body, args) ->
        let bodyKey = bindPath scope lookupExt body
        let bindArg (Model.ApplyArg (path, arg)) =
            Model.ApplyArg (Scope.SubKey(bodyKey, nameOfPath path), bindExpr name scope lookupExt arg)
        in Model.ApplyExpr (bodyKey, List.map bindArg args)

and bindParams innerScope lookupExt parameters =
    let bindParam (Model.Param (path, outerPath, limit)) =
        let pathName = nameOfPath path
        in Model.Param (
            bindPath innerScope lookupExt path,
            Option.bind (fun outerScope -> Scope.lookup outerScope pathName) innerScope.parent,
            Option.map (bindLimit innerScope lookupExt) limit)
    in List.map bindParam parameters
         

let bindDefs scope defs =
    let namedExts = Map.ofList (List.concat (List.map Option.toList (List.map namedExternal defs)))
    let lookupExt name = Map.find name namedExts
    let keyDef (Model.Def (path, _, limit, expr)) =
        Model.Def (bindPath scope lookupExt path, None, Option.map (bindLimit scope lookupExt) limit, Option.map (bindExpr (nameOfPath path) scope lookupExt) expr)
    in List.map keyDef defs

let bind : Scope.Scope option -> Name -> Model.Def<Path<Name>> list -> Model.Def<Key> list =
    fun parentScope scopeName defs -> 
        let scope = Scope.makeScope parentScope scopeName (List.map (function Model.Def(path, _, _, _) -> nameOfPath path) defs)
        in bindDefs scope (extDefs defs)