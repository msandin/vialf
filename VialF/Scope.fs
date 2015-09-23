module Scope


type Scope<'D> = {
    outer :Scope<'D> option
    list :'D list
    index :Map<string, 'D>
}

let empty :Scope<'D> = {outer=None; list=[]; index=Map.empty}

let bind :Scope<'D> -> string -> 'D -> Scope<'D> =
    fun env name d -> { outer = env.outer; list=d::env.list; index = Map.add name d env.index }

let sub :Scope<'D> -> Scope<'D> = fun env -> {outer=Some env; list=[]; index=Map.empty}

let extract :Scope<'D> -> 'D list = fun env -> List.rev env.list 

let rec lookup :Scope<'D> -> string -> 'D option =
    fun env name ->
        match Map.tryFind name env.index with
        | None -> Option.bind (fun parentEnv -> lookup parentEnv name) env.outer 
        | result -> result