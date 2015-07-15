module Syntax

open FParsec
open CharParsers

// Tree
//-//-//-//-//-//-//-//
type Expr = 
    | PathExpr of string
    | NumberExpr of string

type Import = 
    { path : string }

type Role = 
    { name : string
      valueness : bool
      domainArgs : Domain list
      roleExpr : Expr option
      valueExpr : Expr option }

and DomainBody = 
    { domains : Domain list
      roles : Role list }

and DomainExpr = 
    | DomainLiteral of DomainBody
    | DomainDummy

and Domain = 
    { name : string
      expr : DomainExpr }

type Source = 
    { space : string
      imports : Import list
      domains : Domain list }

// Parser
//-//-//-//-//-//-//-//
let lit s = pstring s

let pId str = 
    (let isIdentifierFirstChar c = isLetter c || c = '_'
     let isIdentifierChar c = isLetter c || isDigit c || c = '_'
     many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier") str

let pItem item = spaces >>? item .>> spaces
let pSemiItem item = (pItem item) .>> lit ";"
let pSemiItems item str = many (pSemiItem item) str
let pCommaSep str = (lit "," >>. spaces) str
let pCommaItems item str = sepBy1 (pItem item) pCommaSep str

// header
let pNamespace s = (lit "namespace" >>. spaces1 >>. pId .>> spaces) s
let pImport s = (lit "import" >>. spaces1 >>. pId .>> spaces |>> fun id -> { path = id }) s
let pHeader str = pipe2 (pSemiItem pNamespace) (pSemiItems pImport) (fun n is -> (n, is)) str
// body
let pExpr str = 
    (spaces 
     >>. ((numberLiteral NumberLiteralOptions.None "number" |>> (fun x -> NumberExpr x.String)) 
          <|> (pId |>> (fun x -> PathExpr x)))) str


let pRoleDef str = opt (spaces >>? lit "::" >>. spaces >>. pExpr) str // why the difference?
let pValDef str = opt (spaces >>? lit "=" >>. spaces >>. pExpr) str // to this?

let rec pDomArgDef str = (pId .>> spaces .>> lit "=" .>> spaces .>>. pDomainExpr |>> fun (name, expr) -> {name=name; expr=expr}) str
and pDomArgsDef str = opt (spaces >>? lit "=>" >>. spaces >>. pCommaItems pDomArgDef) str

and pVal str = 
    (opt (lit "val") |>> function 
     | None -> false
     | Some _ -> true) str

and pRole str = 
    (lit "role" >>. spaces1 >>. pVal .>> spaces .>>. pId .>>. pDomArgsDef .>>. pRoleDef .>>. pValDef |>> fun ((((valueness, name), dArgs), rDef), vDef) -> 
         { name = name
           valueness = valueness
           domainArgs = match dArgs with None -> [] | Some sdArg -> sdArg
           roleExpr = rDef
           valueExpr = vDef }) str

and pDomainBody s = 
    (((pSemiItems pDomain) .>>. (pSemiItems pRole)) |>> fun (domains, roles) -> 
         { domains = domains
           roles = roles }) s

and pDomainExpr str = (lit "dex" >>% DomainDummy) str

and pDomainLiteral str = (spaces >>? lit "{" >>. pDomainBody .>> spaces .>> lit "}" |>> DomainLiteral) str

and pDomainRight str = (pDomainLiteral <|> pSemiItem pDomainExpr) str

and pDomain str = 
    (lit "domain" >>. spaces1 >>. pId .>> spaces .>> lit "=" .>>. pDomainRight |>> fun (name, expr) -> 
         { name = name
           expr = expr }) str

let pBody = many (spaces >>? pDomain)

// file
let pVial = 
    pipe2 pHeader pBody (fun (space, imports) domains -> 
        { space = space
          imports = imports
          domains = domains }) .>> spaces .>> eof

let parse str = run pVial str

// next things to do is to
// -) print
// -) domain arguments to roles