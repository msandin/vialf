module Syntax

open FParsec
open CharParsers
open Util


// Tree
//-//-//-//-//-//-//-//
type Path = string list

type Expr = 
    | PathExpr of Path
    | NumberExpr of string

type Import = 
    { path : string }

type Role = 
    { name : string
      def : bool
      valueness : bool
      domainArgs : Domain list
      roleExpr : Expr option
      valueExpr : Expr option }

and DomainBody = 
    { domains : Domain list
      roles : Role list }

and DomainExpr = 
    | DomainRef of Path
    | DomainLiteral of DomainExpr option * DomainBody

and Domain = 
    { name : string
      expr : DomainExpr }

type Source = 
    { space : string
      imports : Import list
      domains : Domain list }

// Parser
//-//-//-//-//-//-//-//

exception ParseFailure 

// combinators
let maybe item str = (opt item |>> function None -> false | _ -> true) str

// primitives

let tok p = (p .>> spaces)  
let tokS p = (p .>> spaces1)
let id str = 
    (let isIdentifierFirstChar c = isLetter c || c = '_'
     let isIdentifierChar c = isLetter c || isDigit c || c = '_'
     many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier") str

// parsers

let pLit token = tok (pstring token)
let pLitS token = tokS (pstring token)

let pId str = tok id str
let pIdS str = tokS id str

let pNumb str = tok (numberLiteral NumberLiteralOptions.None "number") str

let pPath str = sepBy1 pId (pLit ".") str

let pSemiItem item = item .>> pLit ";"
let pSemiItems item = many (pSemiItem item)
let pCommaItems item = sepBy1 item (pLit ",")

// header
let pNamespace s = (pLitS "namespace" >>. pId) s
let pImport s = (pLitS "import" >>. pId |>> fun id -> { path = id }) s
let pHeader str = pipe2 (pSemiItem pNamespace) (pSemiItems pImport) (fun n is -> (n, is)) str

// body
let pExpr str = 
    ((pNumb |>> (fun x -> NumberExpr x.String))  <|> (pPath |>> (fun x -> PathExpr x))) str

let pRoleDef str = opt (pLit "::" >>. pExpr) str 
let pValDef str = opt (pLit "=" >>. pExpr) str

let rec pDomArgDef str = (pId .>> pLit "=" .>>. pDomainExpr |>> fun (name, expr) -> {name=name; expr=expr}) str
and pDomArgsDef str = opt (pLit "=>" >>. pCommaItems pDomArgDef) str

and pRole str = 
    (maybe (pLitS "def") .>> pLitS "role" .>>. maybe (pLitS "val") .>>. pId .>>. pDomArgsDef .>>. pRoleDef .>>. pValDef |>> fun (((((def, valueness), name), dArgs), rDef), vDef) -> 
         { name = name
           def = def
           valueness = valueness
           domainArgs = match dArgs with None -> [] | Some sdArg -> sdArg
           roleExpr = rDef
           valueExpr = vDef }) str

and pDomainBody s = 
    (((many pDomain) .>>. (pSemiItems pRole)) |>> fun (domains, roles) -> 
         { domains = domains
           roles = roles }) s

and pDomainExpr str = (pPath |>> DomainRef) str

and pDomainLiteral str = (pLit "{" >>. pDomainBody .>> pLit "}") str

and pDomainRightEnd str = ((pDomainLiteral |>> Some) <|> (pLit ";" |>> fun _ -> None)) str 

and pDomainRight : CharStream<'a> -> Reply<DomainExpr> =
    fun str -> ((opt pDomainExpr .>>. pDomainRightEnd) |>> function (None, None) -> raise ParseFailure |  (Some e, None) -> e | (e, Some body) -> DomainLiteral (e, body)) str

and pDomain str = 
    (pLitS "domain" >>. pId .>> pLit "=" .>>. pDomainRight |>> fun (name, expr) -> 
         { name = name
           expr = expr }) str

let pBody = many (spaces >>? pDomain)

// file
let pVial = 
    pipe2 (spaces >>. pHeader) pBody (fun (space, imports) domains -> 
        { space = space
          imports = imports
          domains = domains }) .>> eof

let parse str = run pVial str

