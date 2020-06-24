namespace Banach

open Parsy
open Parsy.ParserOperators
open Parsy.TextParserOperators

[<RequireQualifiedAccess>]
module Parser =

    let makeUExpr body = { Body = body ; Annotation = () }


    let oneOrMoreSpaces : int Parser =
        TextParser.character ' ' |> TextParser.oneOrMore |> Parser.ofTextParser |> Parser.map (fun s -> s.Length)


    let bracketed p = !!(!"(" ++ -" ") >>. p .>> !!(-" " ++ !")")


    let exactlyNSpaces (n : int) : unit Parser = !!!System.String(' ', n)


    let identifierPart =
        TextParser.sequence TextParser.letter (TextParser.zeroOrMore TextParser.letterOrDigit)
        |> TextParser.filter (fun s -> s <> "match" && s <> "with")
        |> Parser.ofTextParser


    let identifier : Ident Parser =
        Parser.interleave List.singleton (fun xs () x -> x::xs) identifierPart !!!"." >=> (List.rev >> Ident)


    let hole : Hole Parser =
        !!!"?" >>. identifierPart >=> Hole


    let newLines : unit Parser =
        !!(TextParser.zeroOrMore (-" " ++ TextParser.newLine))


    let rec inlineExpressionInner () : UExpr Parser =

        let ident = identifier >=> (ExprIdent >> makeUExpr)
        let hole = hole >=> (ExprHole >> makeUExpr)

        let bracketedInline =
            let inlineExpression = Parser.delay inlineExpressionInner
            let named = (identifier .>> !!(+" " ++ !":" ++ +" "), inlineExpression) >==> (fun i e -> ExprNamed (i, e) |> makeUExpr)
            Parser.choice [ inlineExpression ; named ] |> bracketed

        let apps =
            let part = Parser.choice [ hole ; ident ; bracketedInline ]
            let makeApp e1 () e2 = ExprApp (e1, e2) |> makeUExpr
            Parser.interleave1 makeApp makeApp part (!!(+" "))

        let arrows =
            let part = Parser.choice [ hole ; ident ; apps ; bracketedInline ]
            Parser.interleave1 (fun e1 () e2 -> [e2;e1]) (fun es () e -> e::es) part !!(+" " ++ !"->" ++ +" ")
            >=> (List.reduce (fun e1 e2 -> ExprArr (e2, e1) |> makeUExpr))

        Parser.choice [ hole ; ident ; apps ; arrows ]


    let inlineExpression : UExpr Parser = inlineExpressionInner ()


    let rec expression (indent : int) : UExpr Parser =

        let matchCase =
            (
                (TextParser.newLine |> Parser.ofTextParser) >>. exactlyNSpaces indent >>. !!(!"|" ++ +" ") >>. identifier,
                Parser.zeroOrMore [] (fun ps p -> ps@[p]) (!!(+" ") >>. identifier),
                !!(+" " ++ !"->" ++ +" ") >>. inlineExpression
            )
            >===> (fun i ps e -> i, ps, e)

        let matchExpression =
            (
                !!(!"match" ++ +" ") >>. inlineExpression .>> !!(+" " ++ !"with"),
                Parser.zeroOrMore [] (fun cs c -> cs@[c]) matchCase
            )
            >==> (fun e cases -> ExprMatch (e, cases) |> makeUExpr)

        Parser.choice [ inlineExpression ; matchExpression ]


    let rec valueDefinition (indent : int) : UValueDef Parser =

        let recursive = Parser.optional !!(!"rec" ++ +" ") >=> (function Some () -> true | None -> false)

        let parameters = Parser.zeroOrMore [] (fun xs x -> xs@[x]) (!!(+" ") >>. bracketed (identifier .>> !!(+" " ++ !":" ++ +" ") .>>. inlineExpression))

        let inlineBody = !!(+" ") >>. inlineExpression >=> (fun e -> [], e)

        let multilineBody =
            let multilineInner indent2 =
                let newIndent = indent + indent2
                definitions newIndent .>>. expression newIndent
            (TextParser.newLine |> Parser.ofTextParser) >>. exactlyNSpaces indent >>. (oneOrMoreSpaces |> Parser.bind multilineInner)

        let body = Parser.choice [ inlineBody ; multilineBody ]

        (
            !!(!"let" ++ +" ") >>. recursive .>>. identifier,
            parameters,
            !!(+" " ++ !":" ++ +" ") >>. inlineExpression,
            !!(+" " ++ !"=") >>. body
        )
        >====>
        (fun (recursive, name) parameters returnType (defs, body) ->
            { Recursive = recursive ; Name = name ; Parameters = parameters ; ReturnType = returnType ; InnerDefinitions = defs ; Body = body ; Annotation = () })


    and typeDefinition (indent : int) : UTypeDef Parser =

        let constructor =
            (TextParser.newLine |> Parser.ofTextParser) >>. exactlyNSpaces indent >>. !!(!"|" ++ +" ") >>. identifier .>> !!(+" " ++ !":" ++ +" ") .>>. inlineExpression

        (
            !!(!"type" ++ +" ") >>. identifier,
            !!(+" " ++ !":" ++ +" ") >>. inlineExpression .>> !!(+" " ++ !"="),
            Parser.zeroOrMore [] (fun xs x -> xs@[x]) constructor
        )
        >===>
        (fun name tType constructors -> { Name = name ; TType = tType ; Constructors = constructors ; Annotation = () })


    and definition (indent : int) : UDef Parser =
        Parser.choice
            [
                valueDefinition indent >=> ValueDef
                typeDefinition indent >=> TypeDef
            ]


    and definitions (indent : int) : UDef list Parser =
        Parser.zeroOrMore [] (fun defs def -> defs@[def]) (definition indent .>> newLines .>> exactlyNSpaces indent)
