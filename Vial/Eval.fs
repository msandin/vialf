namespace Vial

module Eval =

    open Util

    type Name = Syntax.Name
    type Path = Syntax.Path
    type Expr = Syntax.Expr
    type Value =
        | Nothing
        | Text of string
        | Closure of Env * Syntax.Declaration
        | External of (Args -> Value)
    and Args = List<Name * Value>
    and Env = Map<Name, Binding>
    and Binding = { mutable value :Value }

    let binding value = {value=value}

    let external name f = (name, External f)

    let bindings :List<Name * Value> -> Env =
        List.fold (fun e (n, b) -> Map.add n (binding b) e) Map.empty

    let bind :Env -> Name -> Value -> Env =
        fun env name value -> Map.add name (binding value) env

    let bindArgs :Env -> Args -> Env =
        fun env args -> List.fold (fun e (n, v) -> bind e n v) env args


    let bindBlock :Env -> Syntax.Block -> Env =
        // the mutable part here is to deal with recursive lookups between these extended things
        let addBinding e (d:Syntax.Declaration) =
            Map.add (d.name) (binding Nothing) e
        fun env block ->
            let env = List.fold addBinding env block.declarations           
            for dec in block.declarations do
                let binding = Map.find (dec.name) env
                binding.value <- Closure (env, dec)
            env



    let tryLookupName :Env -> Name -> Option<Value> =
        fun env name -> Option.map (fun b -> b.value) (Map.tryFind name env)

    let lookupName :Env -> Name -> Value =
        fun env name -> (Map.find name env).value

    let rec bindContext :Env -> Name -> Option<Path> -> Syntax.Block -> Env =         
        fun env name path block -> // non-recursive binding for now
            let decl = Syntax.context name None block
            let blockEnv = buildContextEnv env path block
            Map.add name (binding (Closure (blockEnv, decl))) env

    and buildContextEnv :Env -> Option<Path> -> Syntax.Block -> Env =
        fun outerEnv parentPath block ->
            let contextEnv =
                match parentPath with
                | Some path ->
                    match lookup outerEnv path with
                    | Closure (_, {children=block}) as v ->
                        List.fold (fun e (d :Syntax.Declaration) -> bind e (d.name) (access v (Path.Final d.name)) ) outerEnv (block.declarations) 
                    | _ -> failwith "Parent context not found."
                | _ -> outerEnv
            bindBlock contextEnv block

    //and rec bindContext2 :Env -> Name -> Syntax.Block -> Env * Syntax.Declaration =
        

    and access :Value -> Path -> Value =
        function 
        | Closure (env, dec) ->
            function 
            | Path.Final name -> // so we have an environment
                let subDec = Syntax.lookup dec.children name // in the closure, lookup the next declaration
                let subEnv = buildContextEnv env (Syntax.parentPath dec.declared) dec.children
                lookupName subEnv name
            | Path.Super (name, subPath) ->
                let subDec = Syntax.lookup dec.children name
                let subEnv = buildContextEnv env (Syntax.parentPath dec.declared) dec.children 
                access (Closure (subEnv, subDec)) subPath
        | _ -> failwith "Not found" // we can look in functions I guess...

    and tryAccess :Value -> Path -> Option<Value> =
        function 
        | Closure (env, dec) ->
            function 
            | Path.Final name -> tryLookupName env name // works because block was bound last              
            | Path.Super (name, subPath) ->
                let subDec = Syntax.lookup dec.children name
                let subEnv = bindBlock env dec.children
                tryAccess (Closure (subEnv, subDec)) subPath
        | _ -> fun _ -> None
    
    and lookup :Env -> Path -> Value =
        fun env ->
            function 
            | Path.Super (name, sub) -> access (lookupName env name) sub
            | Path.Final name -> lookupName env name

    let rec tryLookup :Env -> Path -> Option<Value> =
        fun env ->
            function 
            | Path.Super (name, sub) -> tryAccess (lookupName env name) sub
            | Path.Final name -> tryLookupName env name

    let rec call value args =
        match value with
        | External f -> f args
        | Closure (env, {Syntax.declared=Syntax.Concept(_, Some expr)}) -> eval (bindArgs env args) expr
        | _ -> failwith "Trying to dispatch to a non-function value"

    and eval :Env -> Expr -> Value =
        fun env ->
            function
            | Syntax.Text s -> Text s
            | Syntax.Number s -> Text s
            | Syntax.Access path -> call (lookup env path) []
            | Syntax.Dispatch (path, args) ->
                let value = lookup env path
                let args = List.map (fun (name, expr) -> (name, eval env expr)) args
                call value args


    // when do we want to evaluate things? well, when we build the environment I guess
