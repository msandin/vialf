﻿module Execute

type Tag = Tag of string

type Args = (Tag * Exe) list

and Exe = 
    | DictCallExe of Tag * Args
    | MethCallExe of Exe * Tag * Args
    | FuncCallExe of Exe * Args
    | ConstantExe of Val
    | FuncExe of Exe
    | RecExe of Args
    // we need let, tags, conditionals
    

and Val = 
    | IntVal of int
    | FuncVal of Callable
    | ObjVal of Dict * Val
    | RecVal of Rec

and Dict = Map<Tag, Callable>

and Env = {dict :Dict; values :Map<Tag, Val>}

and Callable = Env * Exe

and Rec = (Tag * Val) list

exception ExeError 

let rec call : Callable -> Rec -> Val =
    fun (callEnv, callExe) argRec ->
        let bindArg values (argName, argVal) =
            Map.add argName argVal values
        let finalCallEnv = {
            dict=callEnv.dict;
            values=List.fold bindArg callEnv.values argRec}
        in execute finalCallEnv callExe

and record : Env -> Args -> Rec =
    fun env args ->
        List.map (fun (argName, argExe) -> (argName, execute env argExe)) args 

and execute : Env -> Exe -> Val =
    fun env -> function
    | ConstantExe value -> value 
    | FuncExe exe -> FuncVal (env, exe)
    | RecExe args -> RecVal (record env args)
    | DictCallExe (funName, args) ->
        let func = Map.find funName env.dict
        in call func (record env args)
    | MethCallExe (exe, methName, args) ->
        match execute env exe with
        | ObjVal (objDict, objVal) ->
            let meth = Map.find methName objDict
            in call meth (record env args)
        | _ -> raise ExeError
    | FuncCallExe (exe, args) -> 
        match execute env exe with
        | FuncVal callable -> call callable (record env args)
        | _ -> raise ExeError
