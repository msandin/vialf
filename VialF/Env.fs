module Env

open System

type Name = String

type Path<'E> =
  | LocalName of Name
  | LocalAccess of Name * Name
  | ExternalAccess of 'E * Name

//type External = External of Name * Guid

type Scope = LocalScope of String * Guid | ExternalScope of Name * Guid

type Key = Key of Scope * Name

let rec externalizePath : Path<Name> -> Path<Scope> =
  function
  | LocalName name -> LocalName name
  | LocalAccess (name, sub) -> LocalAccess (name, sub)
  | ExternalAccess (extName, name) -> ExternalAccess (ExternalScope (extName, Guid.NewGuid ()), name)

let rec keyOfPath : Scope -> (Name -> Scope) -> Path<Scope> -> Key =
  fun local lookupExternal -> function
  | LocalName name -> Key (local, name)
  | LocalAccess (name, sub) -> Key (lookupExternal name, sub)
  | ExternalAccess (ext, name) -> Key (ext, name)

let rec findExternal<'E> : Path<'E> -> Option<'E> =
 function
 | ExternalAccess (ext, _) -> Some ext
 | _ -> None


// type T<'C> = {
//  keys :Map<Key, 'C>
//}
 //| Access (path, name) -> findExternal path


//let lookup<'C> : T<'C> -> Path<Key> -> 'C =
   
  
