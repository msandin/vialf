module Util

let tupleApply f (a,b) = (f a, f b)
let tupleAppend (a,b) = List.append a b
let optionList =
  function
  | Some x -> [x]
  | None -> []