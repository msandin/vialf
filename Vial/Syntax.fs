namespace Vial

module Syntax =

    type Name = string
    type Path = Path.T<Name>

    type Block = {
        declarations :List<Declaration>
        }
    and Declaration = {
        name :string
        children :Block
        declared :Declared
        }
    and Declared =
        | Context of Option<Path>
        | Concept of List<Path> * Option<Expr>
    and Expr =
        | Dispatch of Path * Args
        | Text of string
        | Number of string
        | Access of Path
    and Args = List<Name * Expr>
    

    let rec path =
        function
        | [] -> failwith "Path can't be empty!"
        | head::[] -> Path.Final head
        | head::rest -> Path.Super (head, path rest)

    let parentPath = 
        function
        | Context p -> p
        | _ -> None
         
    let lookup (block :Block) name = 
        List.find (fun (d :Declaration) -> d.name = name) block.declarations

    let block declarations = {declarations = declarations}

    let def name expr block = {name=name; children=block; declared=Concept ([], expr) }

    let context name path block = {name=name; children=block; declared=Context path }

    let access segments = Access (path segments)

    let dispatch segments args = Dispatch (path segments, args)

    let arg name expr = (name, expr)


module Parser =
    open FParsec
    open CharParsers
    open Syntax

    // primitives
    let optElse p d = opt p |>> function None -> d | Some v -> v
    let comment s = (pstring "//" >>. manyTill anyChar (newline <|> (lookAhead eof |>> fun _ -> '\n')) .>> spaces) s
    let tok p = (p .>> spaces .>> many comment)  
    let tokS p = (p .>> spaces1 .>> many comment)
    let id str = 
        let isIdentifierFirstChar c = isLetter c || c = '_'
        let isIdentifierChar c = isLetter c || isDigit c || c = '_'
        many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier" str
    let exact token = tok (pstring token)
    let exactS token = tok (pstring token)
    let number str = tok (numberLiteral NumberLiteralOptions.None "number") str

    // parsers
    let pId str = tok id str
    let pIdS str = tokS id str

    let pPath s :Reply<Path> = (sepBy1 pId (exact ".") |>> path) s

    let pSemiItem item = item .>> exact ";"
    let pSemiItems item = many (pSemiItem item)
    let pCommaItems item = sepBy1 item (exact ",")

    // header
    let pNamespace s = (exactS "namespace" >>. pId) s
    let pImport s = (exactS "import" >>. pId) s
    let pHeader s = pipe2 (pSemiItem pNamespace) (pSemiItems pImport) (fun n is -> (n, is)) s
    
    // block
    let rec pBlock s :Reply<Block> = (exact "{" >>. (many pDeclaration) .>> exact "}" |>> block) s
    and pBlockExpr s = pPath s
    and pContextAssignment s = (exact "=" >>. pBlockExpr .>> exact ";") s
    and pContext s = (exactS "context" >>. pId .>>. opt pContextAssignment .>>. pBlock) s

    // declarations
    and pDeclaration s :Reply<Declaration> = (pDefDecl <|> pContextDecl) s
    and pDefDecl s = (exactS "def" >>. pId .>> exact "=" .>>. pExpr .>> exact ";" .>>. optElse pBlock (block []) |>> fun ((id, c), b) -> def id (Some c) b) s
    and pContextDecl s = (pContext |>> fun ((id, path), c) -> context id path c) s

    // expr
    and pArg s = (pId >>. exact ":" .>>. pExpr) s
    and pDispatchExpr s = (pPath .>>. many pArg |>> function (p, []) -> Syntax.Access p | (p, args) -> Syntax.Dispatch (p, args)) s
    and pNumberExpr s = (number |>> fun n -> Syntax.Number n.String) s
    and pExpr s :Reply<Expr> = (pNumberExpr <|> pDispatchExpr) s

    // language
    let pVial s = (spaces >>. many pContext .>> opt comment .>> eof) s 

    let parse s = run pVial s
