module Util

exception Unfinished

let tupleApply f (a,b) = (f a, f b)
let tupleAppend (a,b) = List.append a b

let curry f a b = f(a,b)
let uncurry f (a,b) = f a b

let unfinished () :'T = raise Unfinished