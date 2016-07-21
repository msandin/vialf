namespace Vial

module Path =

    type T<'t> =
        | Super of 't * T<'t>
        | Final of 't

    let rec concat prefixPath suffixPath = 
        match prefixPath with
        | Super (superName, superPath) -> concat superPath (Super (superName, suffixPath))
        | Final name -> Super (name, suffixPath)

    let append path name =
        concat path (Final name) 


module Environment = 

    type Name = string
    type Path = Path.T<Name>

    type T<'t> = {
        outer :Option<T<'t>>
        index :Map<Name, 't> }

    let rec lookupName name scope =
        match Map.tryFind name scope.index with
        | Some x -> Some x
        | None -> Option.bind (lookupName name) scope.outer

    let lookupPath sub path scope = 
        let rec lookupSubPath sub path scope = 
            match path with
            | Path.Super (name, rest) -> lookupSubPath sub rest (sub name scope)
            | Path.Final name -> Map.tryFind name scope.index
        match path with
        | Path.Super (name, rest) -> lookupSubPath sub rest (sub name scope)
        | Path.Final name -> lookupName name scope