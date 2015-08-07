open FParsec
open Microsoft.FSharp.Collections

type SExpr = Identifier of string
           | BooleanLiteral of bool
           | NumberLiteral of int
           | FloatLiteral of float
           | StringLiteral of string
           | ListLiteral of SExpr list

type UserState = unit
type Parser<'a> = Parser<'a,UserState>

let sbool:Parser<_> =  stringReturn "#t" (BooleanLiteral true)
                   <|> stringReturn "#f" (BooleanLiteral false)
let snumber:Parser<_> = pint32 |>> NumberLiteral
let sfloat:Parser<_> = pfloat |>> FloatLiteral
let sstring:Parser<_> = 
    let isCharcter c = c <> '\"'
    between (pstring "\"" ) (pstring "\"") (spaces >>. (manySatisfy isCharcter) .>> spaces) |>> StringLiteral 

let isSpecialInitialCharacter c = 
    match c with
    | '!' | '$' | '%' | '&' | '*' | '/' | ':' | '<' | '=' | '>' | '?' | '^' | '_' | '~'  -> true
    | _ -> false
let isSpecialSubsequentCharacter c = 
    match c with
    | '+' | '-' | '.' | '@' -> true
    | _ -> false
let isInitialCharacter c = isLetter c || isSpecialInitialCharacter c
let isSubsequentCharacter c = isInitialCharacter c || isDigit c || isSpecialSubsequentCharacter c

let sPeculiarIdentifier:Parser<_> = 
    pstring "+" <|> pstring "-" <|> pstring "..."
let sIdentifier:Parser<_> = 
    sPeculiarIdentifier
    <|> many1Satisfy2 isInitialCharacter isSubsequentCharacter
    |>> Identifier <?> "sIdentifier"

let sExpr,sExprRef = createParserForwardedToRef<SExpr,UserState>()
do sExprRef := 
    let bracket = (between (pstring "(") (pstring ")") (spaces >>. many (sExpr .>> spaces) .>> spaces)) |>> ListLiteral
    choice [
        bracket
        sbool
        sfloat
        snumber
        sstring
        sIdentifier
    ] <?> "sExpr" 

let Lisp = spaces >>. many (sExpr .>> spaces) .>> spaces .>> eof |>> ListLiteral

let test p str = 
    printfn "%s" str
    match run p str with
    | Success(result,_,_) -> printfn "Succeeded: %A" result
    | Failure(msg,_,_) -> printfn "Failed: %s" msg

type LispObject = Float of float
                | String of string
                | Boolean of bool
                | Variable of string
                | List of LispObject list
                | Function of args : LispObject list * body : LispObject list
                | Atom
                | Eq
                | Cons
                | Car
                | Cdr
                | If
                | Cond
                | Quote
                | Lambda
                | Define

let rec eval expr (ctx:Map<string,LispObject>) =
    //Debug Output
    let eval expr ctx = 
        let ret = eval expr ctx
        match (fst ret) with
        | Function(_,_) -> printfn "Function"
        | v -> printfn "%A" v
        ret
    let call (args : LispObject list) body (xs : LispObject list) (ctx : Map<string, LispObject>) = 
        //printfn "callee: %A\nargs: %A" (Function(args,body)) xs
        if args.Length <> xs.Length
        then failwith "call: arity doesn't match"
        else
            let new_ctx = 
                Seq.zip args xs |> Seq.fold (fun ctx (var, obj) -> 
                                        match var with
                                        | Variable(name) -> Map.add name obj ctx
                                        | _ -> failwith "function: arguments must be variables")
                                    ctx
            let ret = eval (List body) new_ctx |> fst
            (ret,ctx)
    //printfn "%A\n%A" expr ctx 
    match expr with
    | Variable str -> 
        //printfn "Variable! %A" ctx.[str]
        printf "var %s -> " str
        (ctx.[str],ctx)
    | List(x::xs) ->
        //printfn "first:%A\nremains%A" x xs
        let value,ctx = eval x ctx
        match value with
        | Atom ->
            match xs with
            | [x] -> 
                match fst (eval x ctx) with
                | List(xs) -> (Boolean false,ctx)
                | v -> (Boolean true,ctx)
            | _ -> failwith "atom: arity must be 1"
        | Eq ->
            match xs with
            | [v1;v2] -> (Boolean ((eval v1 ctx) = (eval v2 ctx)),ctx)
            | _ -> failwith "eq: arity must be 2"
        | Cons ->
            match xs with
            | [x;xs] -> 
                let x,ctx = eval x ctx
                match fst (eval xs ctx) with
                | List(xs) -> (List (x::xs),ctx)
                | _ -> failwith "cons: 2nd argument must be list"
            | _ -> failwith "cons: arity must be 2"
        | Car ->
            match xs with
            | [arg] ->
                match fst (eval arg ctx) with 
                    | List(car::_) -> printfn "car -> %A" car; (car,ctx)
                    | List([]) -> failwith "car: argument shall not be nil"
                    | _ -> failwith "car: argument must be list"
            | _ -> failwith "car: arity must be 1"
            (* match fst (eval (ListObject xs) ctx) with
            | ListObject(car::_) -> (fst (eval car ctx),ctx)
            | ListObject([]) -> failwith "car: argument shall not be nil"
            | _ -> failwith "car: argument must be list" *)
        | Cdr ->
            match xs with
            | [arg] ->
                match fst (eval arg ctx) with 
                    | List(_::cdr) -> printfn "cdr -> %A" cdr; (List cdr,ctx)
                    | List([]) -> failwith "cdr: argument shall not be nil"
                    | _ -> failwith "cdr: argument must be list"
            | _ -> failwith "cdr: arity must be 1"
            (*match xs with
            | [ListObject(xs)] -> 
                match xs with
                | _::cdr -> (ListObject cdr,ctx)
                | [] -> failwith "cdr: argument shall not be nil"
            | [_] -> failwith "cdr: argument must be list"
            | _ -> failwith "cdr: arity must be 1"*)
            (*match fst (eval (ListObject xs) ctx) with
            | ListObject(_::cdr) -> 
                printfn "aaaaaaaaaaaadfghjkhgfdfghjklaaaaaaa\n%A" cdr
                (ListObject cdr,ctx)
            | _ -> failwith "cdr: argument must be list"*)
        | Cond ->
            let value = xs |>
                        List.fold (fun state x -> 
                            match state with
                            | None -> 
                                match x with
                                | List([cond;value]) -> 
                                    match fst (eval cond ctx) with
                                    | Boolean(true) -> Some(fst (eval value ctx))
                                    | Boolean(false) -> None
                                    | _ -> failwith "cond: condition must be boolean"
                                | _ -> failwith "cond: arguments must be (condition value)"
                            | some -> some
                        ) None
            match value with
            | None -> failwith "cond: no condition matched"
            | Some(obj) -> (obj,ctx)
        | If -> 
            match xs with
            | [cond; v1; v2] ->
                match eval cond ctx with
                | (Boolean true,ctx) -> eval v1 ctx
                | (Boolean false,ctx) -> eval v2 ctx
                | _ -> failwith "cond: 2nd value must be bool."
            | _ -> failwith "cond: arity must be 3"
        | Quote -> 
            //printfn "quotes %A" xs
            match xs with
            | [_ as x] -> (x,ctx)
            | [] -> (List [],ctx)
            | _ -> failwith "quote: arity must be 1"
        (* | Variable(_) as var ->  
            printfn "aaaaaaaaaaaaaaaaaaaaaaaaa"
            match fst (eval var ctx) with
            | Function(args,body) -> call args body xs ctx
            | _ -> failwith "variable: 1st element must be function" *)
        | Function(param,body) as f -> 
            let args = xs |> List.map (fun arg -> fst (eval arg ctx))
            call param body args ctx
        | Lambda ->
            match xs with
            | List(args)::body -> (Function(args,body),ctx) (* TODO : implement lexical scope*)
            | _::_ -> failwith "lambda: first argument must be list"
            | _ -> failwith "lambda: arity must be 2 or greater"
        | Define ->
            match xs with
            | [Variable(name);obj] -> (List([]),Map.add name (fst (eval obj ctx)) ctx)
            | [_;_] -> failwith "define: first argument must be variable"
            | _ -> failwith "define: arity must be 2"
        | _ -> //eval (ListObject xs) ctx
            match xs with
            | [] -> (value,ctx)
            | xs -> eval (List xs) ctx
    | obj -> (obj,ctx)

let concatLisp x xs = 
    match xs with
    | List(xs) -> List(x::xs)
    | _ -> failwith "xs must be a ListObject"

let rec preEval expr = 
    match expr with
    | BooleanLiteral b -> Boolean b
    | NumberLiteral n -> Float ((float)n)
    | FloatLiteral f -> Float f
    | StringLiteral s -> String s
    | ListLiteral([]) -> List []
    | ListLiteral(x::xs) -> concatLisp (preEval x) (preEval (ListLiteral xs))
    | Identifier("atom") -> Atom
    | Identifier("eq") -> Eq
    | Identifier("cons") -> Cons
    | Identifier("car") -> Car
    | Identifier("cdr") -> Cdr
    | Identifier("if") -> If
    | Identifier("quote") -> Quote
    | Identifier("cond") -> Cond
    | Identifier("lambda") -> Lambda
    | Identifier("define") -> Define
    | Identifier(str) -> Variable str

[<EntryPoint>]
let main argv =
    let lisp = runParserOnFile Lisp () "lisp2.lisp" System.Text.Encoding.UTF8
    //let lisp = "(quote (quote (1 2 3)))" |> run Lisp
    match lisp with
    | Success(result,_,_) -> printfn "\n%A" (eval (preEval result) (Map.ofList []))
    | Failure(msg,_,_) -> printfn "%s" msg
    0 // return an integer exit code
