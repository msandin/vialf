module Execute


type Args = (string * Exe) list

and Exe = 
  | DictCallExe of string * Args
  | MethCallExe of Exe * Args
  | FuncCallExe of string * Args
  | ConstantExe of Val

and Val = 
  | IntVal of int
  | FuncVal of Callable
  | ObjVal of Dict * Val

and Dict = Map<string, Callable>

and Env = {dict :Dict; values :Map<string, Val>}

and Callable = Env * Exe


exception ExeError 

let rec call : Callable -> Args -> Val =
   fun (env, exe) args -> IntVal 8
 
and execute : Env -> Exe -> Val =
    fun env -> function
    | ConstantExe value -> value 
    | DictCallExe (funName, args) -> raise ExeError
    | MethCallExe (funName, args) -> raise ExeError
    | FuncCallExe (funName, args) -> raise ExeError