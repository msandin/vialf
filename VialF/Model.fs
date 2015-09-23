module Model

open Util

module Location = 
    type Path = EmptyPath | SubPath of Path * string
    type T = {
        space :string
        path :Path
    }
    let rootLocation space = {T.space=space;path=EmptyPath} 
    let subLocation (loc:T) name = {T.space=loc.space;path=SubPath(loc.path, name)}
    let nameFromPath =
        function EmptyPath -> "<unknown>" | SubPath (_,name)  -> name

    let sameName :T -> T -> bool =
        fun a b -> (nameFromPath a.path) = (nameFromPath b.path) 

type DomainExpr =
    | Domain of Domain
    | DomainLiteral of DomainExpr option * DomainBody

and DomainBody = 
    { domains : Domain list
      roles : Role list }

and DomainMember = {
    name :string
    expr :DomainExpr }

and Domain = {
    location :Location.T
    expr :DomainExpr 
    body :DomainBody }

and Role = {
    location :Location.T }

module DomainExprs =

    // extracts domains from expression
    let rec extractDomains :DomainExpr -> Domain list =
        function
        | Domain domain -> domain.body.domains
        | DomainLiteral (innerExpr, body) ->
            match Option.map extractDomains innerExpr with
            | None -> body.domains
            | Some innerDs -> List.append (List.filter (fun d -> not (List.exists (fun (bD :Domain) -> Location.sameName d.location bD.location) body.domains)) innerDs) body.domains 

    // extract roles from expression
    let rec extractRoles :DomainExpr -> Role list =
        function
        | Domain domain -> domain.body.roles
        | DomainLiteral (innerExpr, body) ->
            match Option.map extractRoles innerExpr with
            | None -> body.roles
            | Some innerRs -> List.append (List.filter (fun d -> not (List.exists (fun bR -> Location.sameName d.location bR.location) body.roles)) innerRs) body.roles 


module Domains = 
    type DomainScope = Scope.Scope<Domain>

    let lookupSubdomain  :Domain -> string -> Domain option =
        fun domain name ->
            List.tryFind (fun sub -> (Location.nameFromPath sub.location.path) = name) domain.body.domains

    let rec lookupPath :Domain -> Syntax.Path -> Domain option =
        fun root ->
            function
            | [] -> Some root
            | p::ps -> Option.bind (fun sub -> lookupPath sub ps) (lookupSubdomain root p)

    let rec lookupDomain :DomainScope -> Syntax.Path -> Domain option =
        fun env -> function
        | [] -> None
        | p::ps -> Option.bind (fun domain -> lookupPath domain ps) (Scope.lookup env p)

    // bind a domain expression
    let rec bindDomainExpr :Location.T -> DomainScope -> Syntax.DomainExpr -> DomainExpr option =
        fun loc outerEnv -> function
        | Syntax.DomainRef path -> Option.map (fun x -> Domain x) (lookupDomain outerEnv path)
        | Syntax.DomainLiteral (expr, body) ->
            let innerExpr = Option.bind (bindDomainExpr loc outerEnv) expr
            let subEnv = Scope.sub outerEnv
            let subs = bind loc subEnv body.domains
            let roles = List.map (fun (x :Syntax.Role) -> { location = Location.subLocation loc x.name}) body.roles
            Some (DomainLiteral (innerExpr, { DomainBody.domains = Scope.extract subs; roles = roles } ))
   
    // bind a single domain
    and bindDomain :Location.T -> DomainScope -> Syntax.Domain -> Domain =
        fun outerLoc env dom -> 
            let loc = Location.subLocation outerLoc dom.name
            let expr = Option.get (bindDomainExpr loc env dom.expr)
            // build a domain where we have interpreted the expression and collapsed the domain expressions
            // with regards to binding stuff
            { location = loc
              expr = expr
              body = { domains = DomainExprs.extractDomains expr; roles = DomainExprs.extractRoles expr } }

    // bind a sequence of domains
    and bind :Location.T -> DomainScope -> Syntax.Domain list -> DomainScope =
        fun loc ->
            List.fold (fun env dom -> let d = bindDomain loc env dom in Scope.bind env dom.name d)



module Roles =
    type RoleScope = Scope.Scope<Role>