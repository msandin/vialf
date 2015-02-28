module Env

open System

type Name = String

type Path<'E> =
  | LocalName of Name
  | LocalAccess of Name * Name
  | ExternalAccess of 'E * Name

type External = External of Name * Guid


type Key = LocalKey of Name | ExternalKey of External * Name

type T<'C> = {
  keys :Map<Key, 'C>
}


let rec externalizePath : Path<Name> -> Path<External> =
  function
  | LocalName name -> LocalName name
  | LocalAccess (name, sub) -> LocalAccess (name, sub)
  | ExternalAccess (ext, name) -> ExternalAccess (External (ext, Guid.NewGuid ()), name)

let rec keyOfPath : (Name -> External) -> Path<External> -> Key =
  fun lookup -> function
  | LocalName name -> LocalKey name
  | LocalAccess (name, sub) -> ExternalKey (lookup name, sub)
  | ExternalAccess (ext, name) -> ExternalKey (ext, name)

let rec findExternal<'E> : Path<'E> -> Option<'E> =
 function
 | ExternalAccess (ext, _) -> Some ext
 | _ -> None
 //| Access (path, name) -> findExternal path


//let lookup<'C> : T<'C> -> Path<Key> -> 'C =
   
  
