module Program

open System
open System.Diagnostics
open Util

type Name = Scope.Name
type Key = Scope.Key
type Path<'I> = Scope.Path<'I>
//type Model.Def<'I> = Model.Model.Def<'I>

// - well, refinement of existing roles (e.g. adding valueness or similar) is very much like 
//   creating new ones and limiting them by equality... 
// - and applying constraints in a speclimit is very much related as well...

// so... next up is.. something else again


// signatures: for expressions
// valueness: for  

// scopes: can resolve sub-keys
// so... we want to collect all the scopes and for each scope we want to keep track of the associated sub-scopes, e.g. we want to
// build a scope tree... and an index...


type ScopeNode = ScopeNode of Scope.Key * (Scope.Name * ScopeNode) list

//let keyScopes =
//    let scopeKeyScope =
//        function 
//        | Scope.LocalScopeKey (name, guid) -> Map.add name (ScopeNode ()) Map.empty
//        | Scope.ExternalScopeKey (name, guid) -> Map.empty
//    in function
//    | Scope.Key

//let limitScope :Model.Limit<Key> -> Map<Key, ScopeNode> =
//    function
//    | Model.EqLimit key -> 
//    | Model.SpecLimit (key, args) -> 

//let scopeNodes :Model.Def<Scope.Key> list -> (ScopeNode * Map<Scope.Key, ScopeNode>) list =
    


[<EntryPoint>]
let main argv = 

    let defs : Model.Def<Path<Name>> list = [
        Model.Def (Scope.LocalName("a"), None, Some (Model.EqLimit(Scope.LocalName("eList"))), None);
        Model.Def (Scope.LocalName("eList"), None, Some (Model.EqLimit(Scope.ExternalAccess("List", "list"))), None);
        Model.Def (Scope.LocalName("eLength"), None, Some (Model.EqLimit(Scope.LocalAccess("eList", "length"))), None);
        Model.Def (Scope.LocalName("dList"), None, Some (Model.SpecLimit(Scope.ExternalAccess("List", "list"), [ Model.Arg (Scope.LocalName("element"), Model.EqLimit(Scope.LocalName("a")))])), None);
        Model.Def (Scope.LocalName("dLength"), None, Some (Model.EqLimit(Scope.LocalAccess("dList", "length"))), None);
        Model.Def (Scope.LocalName("b"), None, None, Some (Model.FunExpr([Model.Param(Scope.LocalName("a"), None, None)], Model.RefExpr(Scope.LocalName("a")))));
        Model.Def (Scope.LocalName("c"), None, None, Some (Model.ApplyExpr(Scope.LocalName("b"), [Model.ApplyArg(Scope.LocalName("a"), Model.RefExpr(Scope.LocalName("a")))])))]
    let result = Bind.bind None "" defs
    Debug.Print (sprintf "Defs:\n%A" defs)
    Debug.Print (sprintf "Result:\n%A\n" result)
    0
