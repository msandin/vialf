module Model

open Util

module Location = 
    type Path = EmptyPath | SubPath of Path * string
    type Location = {
        space :string
        path :Path
    }
    let rootLocation space = {Location.space=space;path=EmptyPath} 
    let subLocation (loc:Location) name = {Location.space=loc.space;path=SubPath(loc.path, name)}
    let nameFromPath =
        function EmptyPath -> "<unknown>" | SubPath (_,name)  -> name


type DomainExpr =
    | Domain of Domain
    | DomainLiteral of DomainExpr option * DomainBody

and DomainBody = 
    { domains : Domain list }

and DomainMember = {
    name :string
    expr :DomainExpr
}

and Domain = {
    location :Location.Location
    expr :DomainExpr // source expression
    body :DomainBody // the body as far as we've built it so far
}
and Role = {
    location :Location.Location
}

module Domains = 
    type DomainScope = Scope.Scope<Domain>

    let lookupSubdomain  :Domain -> string -> Domain option =
        fun domain name -> List.tryFind (fun sub -> (Location.nameFromPath sub.location.path) = name) domain.body.domains

    let rec lookupPath :Domain -> Syntax.Path -> Domain option =
        fun root ->
            function
            | [] -> Some root
            | p::ps -> Option.bind (fun sub -> lookupPath sub ps) (lookupSubdomain root p)

    let rec lookupDomainInEnv :DomainScope -> Syntax.Path -> Domain option =
        fun env -> function
        | [] -> None
        | p::ps -> Option.bind (fun domain -> lookupPath domain ps) (Scope.lookup env p)

    let rec domainExpr :Location.Location -> DomainScope -> Syntax.DomainExpr -> DomainExpr option =
        fun loc outerEnv -> function
        | Syntax.DomainRef path -> Option.map (fun x -> Domain x) (lookupDomainInEnv outerEnv path)
        | Syntax.DomainLiteral (expr, body) ->
            let innerExpr = Option.bind (domainExpr loc outerEnv) expr
            let subEnv = Scope.sub outerEnv
            let subs = bind loc subEnv body.domains
            Some (DomainLiteral (innerExpr, { DomainBody.domains = Scope.extract subs } ))

    and sameName :Domain -> Domain -> bool =
        fun a b -> (Location.nameFromPath a.location.path) = (Location.nameFromPath b.location.path) 

    and subDomains :DomainExpr -> Domain list =
        function
        | Domain domain -> domain.body.domains
        | DomainLiteral (innerExpr, body) ->
            match Option.map subDomains innerExpr with
            | None -> body.domains
            | Some innerDs -> List.append (List.filter (fun d -> not (List.exists (fun bD -> sameName d bD) body.domains)) innerDs) body.domains // maybe we should have some sort of domain bindings here instead

    and bindDomain :Location.Location -> DomainScope -> Syntax.Domain -> Domain =
        fun outerLoc env dom -> 
            let loc = Location.subLocation outerLoc dom.name
            let expr = Option.get (domainExpr loc env dom.expr) // we gotta build a new env, don't we?
            { location = loc
              expr = expr
              body = { domains = subDomains expr } } // we will collect these from the body... right?

    and bind :Location.Location -> DomainScope -> Syntax.Domain list -> DomainScope =
        fun loc ->
            List.fold (fun env dom -> let d = bindDomain loc env dom in Scope.bind env dom.name d)



module Roles =
    type RoleScope = Scope.Scope<Role>