module Program

open System
open System.Diagnostics
open Util

type Name = Scope.Name
type Path<'I> = Scope.Path<'I>
//type Model.Def<'I> = Model.Model.Def<'I>

// - well, refinement of existing roles (e.g. adding valueness or similar) is very much like 
//   creating new ones and limiting them by equality... 
// - and applying constraints in a speclimit is very much related as well...

// so... next up is.. something else again
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
