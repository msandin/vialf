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

type Binding<'a> = {
    content :'a
    prev :Binding<'a> option}

and DomainExpr =
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
    subs :Binding<Domain> list
    roles: Role list }

and Role = {
    location :Location.T }

module DomainExprs =

    // extracts domains from expression
    let rec extractDomains :DomainExpr -> Binding<Domain> list =
        function
        | Domain domain -> domain.subs
        | DomainLiteral (innerExpr, body) ->            
            match Option.map extractDomains innerExpr with
            | None -> List.map (fun domain -> {content=domain; prev=None}) (body.domains)
            | Some innerBindings ->
                let binding (sub :Domain) = {
                    content=sub;
                    prev=List.tryFind (fun x -> Location.sameName sub.location x.content.location) innerBindings}
                let outerBindings = List.map binding body.domains
                let isInOuter (binding :Binding<Domain>) = 
                    List.exists (fun (outer :Binding<Domain>)  -> Location.sameName binding.content.location outer.content.location) outerBindings
                let filteredInnerBindings = List.filter (fun inner -> not (isInOuter inner)) innerBindings
                List.append filteredInnerBindings outerBindings

    // extract roles from expression
    let rec extractRoles :DomainExpr -> Role list =
        function
        | Domain domain -> domain.roles
        | DomainLiteral (innerExpr, body) ->
            match Option.map extractRoles innerExpr with
            | None -> body.roles
            | Some innerRs -> List.append (List.filter (fun d -> not (List.exists (fun bR -> Location.sameName d.location bR.location) body.roles)) innerRs) body.roles 


module Domains = 

    type DomainScope = Scope.Scope<Domain>

    let lookupSubdomain  :Domain -> string -> Domain option =
        fun domain name ->
            Option.map (fun sub -> sub.content) (List.tryFind (fun sub -> (Location.nameFromPath sub.content.location.path) = name) domain.subs)

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
              subs = DomainExprs.extractDomains expr
              roles = DomainExprs.extractRoles expr }

    // bind a sequence of domains
    and bind :Location.T -> DomainScope -> Syntax.Domain list -> DomainScope =
        fun loc ->
            List.fold (fun env dom -> let d = bindDomain loc env dom in Scope.bind env dom.name d)



module Roles =
    type RoleScope = Scope.Scope<Role>