module Model

type Name = Scope.Name
type Key = Scope.Key
type Path<'I> = Scope.Path<'I>

type Arg<'I> = Arg of 'I * Limit<'I>

and Limit<'I> = 
    | EqLimit of 'I
    | SpecLimit of 'I * Arg<'I> list    

type Param<'I> = Param of 'I * 'I option * Limit<'I> option

type Expr<'I> = 
    | RefExpr of 'I
    | FunExpr of Param<'I> list * Expr<'I>
    | ApplyExpr of 'I * ApplyArg<'I> list

and ApplyArg<'I> = ApplyArg of 'I * Expr<'I>

type Def<'I> = Def of 'I * 'I option * Limit<'I> option * Expr<'I> option

type Compiled = Compiled

type Domain = 
    { name : Name
      defs : Map<Name, Compiled> }