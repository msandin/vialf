module Env

open System

type Name = String

type Path<'E> =
    | LocalName of Name
    | LocalAccess of Name * Name
    | ExternalAccess of 'E * Name

//type External = External of Name * Guid

type ScopeKey = LocalScopeKey of Name * Guid | ExternalScopeKey of Name * Guid

type Key = Key of ScopeKey * Name

let rec externalizePath : Path<Name> -> Path<ScopeKey> =
    function
    | LocalName name -> LocalName name
    | LocalAccess (name, sub) -> LocalAccess (name, sub)
    | ExternalAccess (extName, name) -> ExternalAccess (ExternalScopeKey (extName, Guid.NewGuid ()), name)

let rec keyOfPath : ScopeKey -> (Name -> ScopeKey) -> Path<ScopeKey> -> Key =
    fun local lookupExternal -> function
    | LocalName name -> Key (local, name)
    | LocalAccess (name, sub) -> Key (lookupExternal name, sub)
    | ExternalAccess (ext, name) -> Key (ext, name)

let rec findExternal<'E> : Path<'E> -> Option<'E> =
    function
    | ExternalAccess (ext, _) -> Some ext
    | _ -> None

type Scope<'C> = {
    key :ScopeKey
    parent :Option<Scope<'C>>    
    content :Map<Key, 'C>
}

let rec lookup<'C> :Scope<'C> -> Key -> Option<'C> =
    fun scope key ->
    match Map.tryFind key scope.content with
    | Some(c) as result -> result
    | None -> Option.bind (fun parentScope -> lookup scope key) scope.parent

let rootScope<'C> :ScopeKey -> List<string * 'C> -> Scope<'C> =
    fun key fields ->
        { key = key;
          parent = None;
          content = Map.ofList (List.map (fun (name, c) -> (Key(key,name), c)) fields) } 
let subScope<'C> :Scope<'C> -> ScopeKey -> List<string * 'C> -> Scope<'C> =
    fun parent key fields ->
        { key = key;
          parent = Some(parent);
          content = Map.ofList (List.map (fun (name, c) -> (Key(key,name), c)) fields) }  

let scopeName<'C> :Option<Scope<'C>> -> string =
    function
    | None -> "/"
    | Some(scope) -> 
        match scope.key with
        | LocalScopeKey (name, _) -> name
        | ExternalScopeKey (name, _) -> "<" + name + ">"

// type T<'C> = {
//  keys :Map<Key, 'C>
//}
 //| Access (path, name) -> findExternal path


//let lookup<'C> : T<'C> -> Path<Key> -> 'C =
   
  
