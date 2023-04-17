module ImpParser

    open Eval
    open Types

    (*

    The interfaces for JParsec and FParsecLight are identical and the implementations should always produce the same output
    for successful parses although running times and error messages will differ. Please report any inconsistencies.

    *)

    open JParsec.TextParser             // Example parser combinator library. Use for CodeJudge.
    // open FParsecLight.TextParser     // Industrial parser-combinator library. Use for Scrabble Project.
    
    let pIntToChar  = pstring "intToChar"
    let pPointValue = pstring "pointValue"

    let pCharToInt  = pstring "charToInt"
    let pToUpper    = pstring "toUpper"
    let pToLower    = pstring "toLower"
    let pCharValue  = pstring "charValue"

    let pTrue       = pstring "true"
    let pFalse      = pstring "false"
    let pIsDigit    = pstring "isDigit"
    let pIsLetter   = pstring "isLetter"
    let pIsVowel   = pstring "isVowel"

    let pif       = pstring "if"
    let pthen     = pstring "then"
    let pelse     = pstring "else"
    let pwhile    = pstring "while"
    let pdo       = pstring "do"
    let pdeclare  = pstring "declare"

    let pletter          = satisfy System.Char.IsLetter <?> "letter"
    let palphanumeric  = satisfy System.Char.IsLetterOrDigit <?> "alphanumeric"
    let whitespaceChar = satisfy System.Char.IsWhiteSpace <?> "whitespace"

    let spaces         = many whitespaceChar <?> "space"
    let spaces1        = many1 whitespaceChar <?> "space1"

    let (.>*>.) p1 p2 =  p1 .>> spaces .>>. p2
    let (.>*>) p1 p2  = p1 .>> spaces .>> p2
    let (>*>.) p1 p2  = p1 .>> spaces >>. p2

    let parenthesise p = pchar '(' >*>. p .>*> pchar ')'
    let curlyBrackets p = pchar '{' >*>. p .>*> pchar '}'

    let pid = pletter <|> pchar '_' .>>. many (palphanumeric <|> pchar '_') |>> fun (c, cl) -> string c + System.String.Concat(Array.ofList(cl))
    
    let unop (a: Parser<'a>) (b: Parser<'b>) = a .>*>. b |>> fun (a, b) -> b

    let binop op a b = a .>*>. op .>*>. b |>> fun ((a, _), b) -> a, b

    let TermParse, tref = createParserForwardedToRef<aExp> ()
    let ProdParse, pref = createParserForwardedToRef<aExp> ()
    let AtomParse, aref = createParserForwardedToRef<aExp> ()
    let CtomParse, cref = createParserForwardedToRef<cExp> ()
        
    let AddParse = binop (pchar '+') ProdParse TermParse |>> Add <?> "Add"
    let SubParse = binop (pchar '-') ProdParse TermParse |>> Sub <?> "Sub"
    do tref := choice [AddParse; SubParse; ProdParse]

    let MulParse = binop (pchar '*') AtomParse ProdParse |>> Mul <?> "Mul"
    let DivParse = binop (pchar '/') AtomParse ProdParse |>> Div <?> "Div"
    let ModParse = binop (pchar '%') AtomParse ProdParse |>> Mod <?> "Mod"
    do pref := choice [MulParse; DivParse; ModParse; AtomParse]

    let NParse = pint32 |>> N <?> "Int"
    let ParParse = parenthesise TermParse
    let NegParse = unop (pchar '-') AtomParse |>> (fun x -> Mul(N -1, x)) <?> "Neg"
    let VarParse = pid |>> V <?> "Var"
    let PVParse = unop pPointValue ParParse |>> PV <?> "PV"
    let CharToIntParse = unop pCharToInt (parenthesise CtomParse) |>> CharToInt <?> "CharToInt"
    do aref := choice [NegParse; CharToIntParse; PVParse; ParParse; NParse; VarParse]

    let AexpParse = TermParse

    let CParse = pchar '\'' >>. anyChar .>> pchar '\'' |>> C <?> "C"
    let CVParse = unop pCharValue ParParse |>> CV <?> "CV"
    let ToLowerParse = unop pToLower (parenthesise CtomParse) |>> ToLower <?> "ToLower"
    let ToUpperParse = unop pToUpper (parenthesise CtomParse) |>> ToUpper <?> "ToUpper"
    let IntToCharParse = unop pIntToChar ParParse |>> IntToChar <?> "IntToChar"
    do cref := choice [CParse; CVParse; ToUpperParse; ToLowerParse; IntToCharParse]

    let CexpParse = CtomParse

    let BexpParse = pstring "not implemented"

    let stmntParse = pstring "not implemented"


    let parseSquareProg _ = failwith "not implemented"

    let parseBoardProg _ = failwith "not implemented"

    let mkBoard (bp : boardProg) : board = failwith "not implemented"

