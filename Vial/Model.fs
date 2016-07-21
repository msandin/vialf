namespace Vial

module Model = 

    type Name = Syntax.Name
    type Path = Syntax.Path

    type Binding = { // so the point is
        name :Name 
        env :Env}

    and Env = string
        




//namespace Vial
//
//module Model =
//
//    type Name = Environment.Name
//    type Path = Path.T<Name>
//
//    type Id = System.Guid
//    
//    type Env = Environment.T<Id>
//
//    type Block = {
//        declarations :List<Declaration>
//        env :Env
//        }
//    and Declaration = {
//        path :Path
//        id :Id
//        children :Block
//        }
//
//    let rec buildDeclaration outerPath (env :Option<Env>) (id, declaration:Syntax.Declaration) =
//        let path = Path.append outerPath declaration.name
//        let children = buildBlock path env declaration.children
//        {path=path; id=id; children=children}
//
//    and buildBlock path outerEnv (syntaxBlock :Syntax.Block) :Block =
//        let decsWithId = List.map (fun d -> System.Guid.NewGuid(), d) syntaxBlock.declarations
//        let localEnv = {
//            Environment.outer=outerEnv
//            Environment.index=List.fold (fun map (id, d :Syntax.Declaration) -> Map.add (d.name) id map) Map.empty decsWithId }
//        let childDeclarations =
//            List.map (buildDeclaration path (Some localEnv)) decsWithId
//        { declarations=childDeclarations; env=localEnv} 
//
//
//module Interpret = 
//
//    type Name = Environment.Name
//
//    type Value =
//        | Nothing
//        | Text of string
//        | Expr of Syntax.Expr
//        | External of (Args -> Value)
//    and Args = List<Name * Value>
//    and Scope = Scope of Map<Name, Binding>
//    and Record = Struct of Map<Name, Binding>
//    and Binding = {
//            value :Option<Value>
//            mutable record :Option<Record> 
//            }
//
//    // I have to go from ENV -> struct, but I have to construct ENV before struct, but I only get away with
//    // having a static env because I have to mutable variables, otherwise I would need proper activasion record
//
//    // but.. for evaluating expressions we should have the environment... and we might want those... but that leaves
//    // the possibility that we are talking about two environments, the lexical environment and the dynamic environment,
//    // but my current scheme only allows the expression to see a static environment, but when we access something through
//    // a value, e.g. a passed in TABLE, we need to so 
//
//    // the bindings should not contain the struct
//
//
////    module Struct = 
////        
////        type T = Map<Name, Value>
////
////        let access t name :Value = Map.find name t
////
////        let tryAccess t name :Option<Value> = Map.tryFind name t
////
////        let rec accessPath t =
////            function
////            | Path.Final name -> access t name
////            | Path.Super (name, sub) -> lookupPath (access env name) sub
//
//    module Env =
//
//        type T = { bindings :Map<Name, Binding> }
//
//        and Binding = {
//            value :Option<Value>
//            mutable table :Option<T> 
//            }
//
//        let access t name :Binding = Map.find name t.bindings
//
//        let tryAccess t name :Option<Binding> = Map.tryFind name t.bindings
//
//        let lookupName name =
//            function
//            | { table = Some t } -> access t name
//            | _ -> failwith (sprintf "No such child %s" (name.ToString()))
//
//        let rec lookupPath (binding :Binding) :Path.T<Name> -> Binding =
//            function 
//            | Path.Super (name, sub) -> lookupPath (lookupName name binding) sub
//            | Path.Final name -> lookupName name binding
//
//        let rec accessPath t path =
//            let binding =
//                match path with
//                | Path.Final name -> access t name
//                | Path.Super (name, sub) -> lookupPath (access t name) sub
//            match binding.value with
//            | Some value -> value
//            | None -> failwith (sprintf "Not a value %s" (path.ToString()))
//
//        let bindArgs env args =
//            List.fold (fun e (n, v) -> {bindings = Map.add n ({value=Some v; table=None}) e.bindings}) env args
//        
//        let bindings bindings =
//            List.fold (fun e (n, b) -> {bindings = Map.add n b e.bindings}) {bindings=Map.empty} bindings
//
//        let external name f = (name, { value=Some (External f); table=None })
//
//        let bindContext env name contextEnv = { bindings=Map.add name { value=None; table=Some(contextEnv) } (env.bindings) } 
//
//    let rec buildBlockEnv (outerEnv :Env.T) (block :Syntax.Block) =
//        let buildValue =
//            function
//            | Syntax.Context _ -> None // at this point we should build the environment
//            | Syntax.Concept (_, Some(Syntax.Text t)) -> Some (Text t)
//            | Syntax.Concept (_, Some(Syntax.Number t)) -> Some (Text t)
//            | Syntax.Concept (_, e) -> Option.map (fun x -> Expr x) e
//        let bind e (d :Syntax.Declaration) =
//            Map.add (d.name) {Env.value = buildValue d.declared; Env.table = None} e  
//        let localEnv = { Env.bindings = List.fold bind (outerEnv.bindings) block.declarations }
//        for declaration in block.declarations do
//            let binding = Env.access localEnv (declaration.name)
//            binding.table <- Some (buildBlockEnv localEnv declaration.children)
//        localEnv
//
//
//    let rec evalExpr env expr args =
//        match expr with
//        | External f -> f args
//        | Expr e -> eval (Env.bindArgs env args) e
//        | _ -> failwith "Trying to dispatch to a non-function value"
//
//    and eval env =
//        function
//        | Syntax.Text s -> Text s
//        | Syntax.Number s -> Text s
//        | Syntax.Access path -> Env.accessPath env path
//        | Syntax.Dispatch (path, args) ->
//            let value = Env.accessPath env path
//            let args = List.map (fun (name, expr) -> (name, eval env expr)) args
//            evalExpr env value args
////            match value with
////            | Env.External f -> f args
////            | Env.Expr e -> eval (Env.bindArgs env args) e
////            | _ -> failwith "Trying to dispatch to a non-function value"
//            // so... we have the value, but the real point here was that the external should be able to dispatch
//            // based on the value, e.g. by looking things up in the value... to be able to do that it needs 
//            // the name is already implicit in the value so that shouldn't be a problem
//        