module Scope

open System

type Name = String

type Path<'E> =
    | LocalName of Name
    | LocalAccess of Name * Name
    | ExternalAccess of 'E * Name

type ScopeKey = LocalScopeKey of Name * Guid | ExternalScopeKey of Name * Guid

type Key = Key of ScopeKey * Name

type Scope = {
    key :ScopeKey
    parent :Option<Scope>    
    content :Set<Name>
}

let rec externalizePath : Path<Name> -> Path<ScopeKey> =
    function
    | LocalName name -> LocalName name
    | LocalAccess (name, sub) -> LocalAccess (name, sub)
    | ExternalAccess (extName, name) -> ExternalAccess (ExternalScopeKey (extName, Guid.NewGuid ()), name)

let rec findExternal<'E> : Path<'E> -> Option<'E> =
    function
    | ExternalAccess (ext, _) -> Some ext
    | _ -> None

let scopeName :Option<Scope> -> string =
    function
    | None -> ""
    | Some(scope) -> 
        match scope.key with
        | LocalScopeKey (name, _) -> name
        | ExternalScopeKey (name, _) -> "<" + name + ">"

let makeScope :Scope option -> Name -> List<string> -> Scope =    
    fun parent name fields ->
        let scopeKey = LocalScopeKey ((scopeName parent) + name + "/", Guid.NewGuid ()) 
        in { key = scopeKey;
             parent = parent;
             content = Set.ofList fields; }  

let rec lookup :Scope -> Name -> Option<Key> =
    fun scope name ->
    if Set.contains name scope.content
    then Some (Key(scope.key, name))
    else Option.bind (fun parentScope -> lookup scope name) scope.parent

let rec keyOfPath : Scope -> (Name -> ScopeKey) -> Path<ScopeKey> -> Key option =
    fun scope lookupExternal -> function
    | LocalName name -> lookup scope name
    | LocalAccess (name, sub) -> Option.bind (fun key -> Some(key)) (lookup scope name)
    | ExternalAccess (ext, name) -> Some(Key (ext, name))