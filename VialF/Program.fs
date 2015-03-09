module Program

open System
open System.Diagnostics
open Util

type Name = Scope.Name
type Key = Scope.Key
type Path<'I> = Scope.Path<'I>

type Arg<'I> = Arg of 'I * Limit<'I>

and Limit<'I> = 
    | EqLimit of 'I
    | SpecLimit of 'I * Arg<'I> list    

type Param<'I> = Param of 'I * 'I option * Limit<'I> option

type Expr<'I> = 
    | RefExpr of 'I
    | FunExpr of Param<'I> list * Expr<'I>

type Def<'I> = Def of 'I * 'I option * Limit<'I> option * Expr<'I> option

type Compiled = Compiled

type Domain = 
    { name : Name
      defs : Map<Name, Compiled> }

exception CompilerFailure of string


let nameOfPath =
    function
    | Scope.LocalName name -> name
    | path -> raise (CompilerFailure (sprintf "In params position: %A" path))

// externals
let namedExternal : Def<Scope.Path<Scope.ScopeKey>> -> (Name * Scope.ScopeKey) option = 
    function 
    | Def(defPath, _, Some(EqLimit(path)), _) | Def(defPath, _, Some(SpecLimit(path, _)), _) -> 
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
    | EqLimit path -> EqLimit(extPath path)
    | SpecLimit(path, args) -> 
        SpecLimit(
            extPath path,
            List.map (function Arg(a, b) -> Arg (extPath a, extLimit b)) args)

let rec extExpr = 
    function 
    | RefExpr path -> RefExpr(extPath path)
    | FunExpr (parameters, body) ->
        FunExpr(
            List.map (function (Param (path, _, limit)) -> Param (extPath path, None, Option.map extLimit limit)) parameters,
            extExpr body)

let extDefs =            
    let extDef (Def(path, _, limit, expr)) =
        Def(extPath path, None, Option.map extLimit limit, Option.map extExpr expr)
    List.map extDef 

// bind
let bindPath scope lookupExt path =
    match Scope.keyOfPath scope lookupExt path with
    | Some key -> key
    | None -> raise (CompilerFailure (sprintf "Not found: %A" path))

let rec bindLimit scope lookupExt =
    let keyOfPath = bindPath scope lookupExt
    function
    | EqLimit path -> EqLimit (keyOfPath path)
    | SpecLimit (path, args) -> 
        let pathKey = keyOfPath path
        let keyOfParam = fun paramPath ->
            let paramName = nameOfPath paramPath
            match pathKey with
            | Scope.Key (Scope.LocalScopeKey(_), _) -> Scope.Key (scope.key, paramName)
            | Scope.Key (Scope.ExternalScopeKey(_, _) as externalScopeKey, _) -> Scope.Key (externalScopeKey, paramName)
        in SpecLimit (pathKey, List.map (function Arg (a, b) -> Arg (keyOfParam a, bindLimit scope lookupExt b)) args)

let rec bindExpr name scope lookupExt = 
    function
    | RefExpr path -> RefExpr (bindPath scope lookupExt path)
    | FunExpr (parameters, body) ->
        let names = List.map (function Param(path, _, limit) -> nameOfPath path) parameters
        let innerScope = Scope.makeScope (Some scope) name names
        in FunExpr (bindParams innerScope lookupExt parameters, bindExpr name innerScope lookupExt body)

// application expression are going to be a lot of fun, because it involves deciding the rules for chosing
// the appropriate argument to bind to and that ultimately involves... hmmm.. can't really decide that here
// because that involves looking stuff up which is impossible for externals... is this crucial to do here?
// i'd like to say 'no' but i feel uncertain.


and bindParams innerScope lookupExt parameters =
    let bindParam (Param (path, outerPath, limit)) =
        let pathName = nameOfPath path
        in Param (
            bindPath innerScope lookupExt path,
            Option.bind (fun outerScope -> Scope.lookup outerScope pathName) innerScope.parent,
            Option.map (bindLimit innerScope lookupExt) limit)
    in List.map bindParam parameters
         

let bindDefs scope defs =
    let namedExts = Map.ofList (List.concat (List.map Option.toList (List.map namedExternal defs)))
    let lookupExt name = Map.find name namedExts
    let keyDef (Def (path, _, limit, expr)) =
        Def (bindPath scope lookupExt path, None, Option.map (bindLimit scope lookupExt) limit, Option.map (bindExpr (nameOfPath path) scope lookupExt) expr)
    in List.map keyDef defs

let bind : Scope.Scope option -> Name -> Def<Path<Name>> list -> Def<Key> list =
    fun parentScope scopeName defs -> 
        let scope = Scope.makeScope parentScope scopeName (List.map (function Def(path, _, _, _) -> nameOfPath path) defs)
        in bindDefs scope (extDefs defs)

// - well, refinement of existing roles (e.g. adding valueness or similar) is very much like 
//   creating new ones and limiting them by equality... 
// - and applying constraints in a speclimit is very much related as well...

[<EntryPoint>]
let main argv = 

    let defs : Def<Path<Name>> list = [
        Def (Scope.LocalName("a"), None, Some (EqLimit(Scope.LocalName("eList"))), None);
        Def (Scope.LocalName("eList"), None, Some (EqLimit(Scope.ExternalAccess("List", "list"))), None);
        Def (Scope.LocalName("eLength"), None, Some (EqLimit(Scope.LocalAccess("eList", "length"))), None);
        Def (Scope.LocalName("dList"), None, Some (SpecLimit(Scope.ExternalAccess("List", "list"), [ Arg (Scope.LocalName("element"), EqLimit(Scope.LocalName("a")))])), None);
        Def (Scope.LocalName("dLength"), None, Some (EqLimit(Scope.LocalAccess("dList", "length"))), None);
        Def (Scope.LocalName("b"), None, None, Some (FunExpr([Param(Scope.LocalName("a"), None, None)], RefExpr(Scope.LocalName("a")))))]
    let result = bind None "" defs
    Debug.Print (sprintf "Defs:\n%A" defs)
    Debug.Print (sprintf "Result:\n%A\n" result)
    0 
