namespace Banach.Test

open Banach
open Parsy
open Xunit

module TestParser =

    let newLine = System.Environment.NewLine

    let assertOneFullParse (parser : 'a Parser) (input : string) (f : 'a -> unit) =
        let parse = ReferenceParser.make parser
        let fullParses = input |> parse |> List.filter (fun (_, s) -> s = "")
        Assert.Equal(1, fullParses |> List.length)
        fullParses |> List.exactlyOne |> fst |> f


    [<Theory>]
    [<InlineData("foo")>]
    [<InlineData("f00")>]
    let ``Valid identifiers parse correctly`` (identifier : string) =
        let test ident = Assert.Equal (identifier.Split('.') |> List.ofArray |> UIdent, ident)
        assertOneFullParse Parser.identifier identifier test


    let i = List.singleton >> UIdent >> UExprIdent
    let app e1 e2 = UExprApp (e1, e2)
    let arr e1 e2 = UExprArr (e1, e2)

    let validExpressionData =
        [
            "a", i "a"
            "a b c", app (app (i "a") (i "b")) (i "c")
            "foo (a b c) bar", app (app (i "foo") (app (app (i "a") (i "b")) (i "c"))) (i "bar")
            "a (foo) (bar b c)", app (app (i "a") (i "foo")) (app (app (i "bar") (i "b")) (i "c"))
            "a -> b", arr (i "a") (i "b")
            "a -> b -> c", arr (i "a") (arr (i "b") (i "c"))
        ]
        |> Map.ofList

    let validExpressions =
        validExpressionData |> Map.toSeq |> Seq.map (fst >> box >> Array.singleton)

    [<Theory>]
    [<MemberData("validExpressions")>]
    let ``Valid expressions parse correctly`` (expression : string) =
        let test e = Assert.Equal(validExpressionData |> Map.find expression, e)
        assertOneFullParse Parser.inlineExpression expression test


    [<Fact>]
    let ``Test value definition parse`` () =
        let input = "let fst (t : Type) (a : t) (b : t) : t = a"
        let test = ignore
        assertOneFullParse (Parser.valueDefinition 0) input test


    [<Fact>]
    let ``Test value definition parse 2`` () =
        let input = "let fst (t : Type) (a : t) (b : t) : t =" + newLine + "    a"
        let test = ignore
        assertOneFullParse (Parser.valueDefinition 0) input test


    [<Fact>]
    let ``Test type definition parse`` () =
        let input = "type Nat : Type =" + newLine + "| Z : Nat" + newLine + "| S : Nat -> Nat"
        let test = ignore
        assertOneFullParse (Parser.typeDefinition 0) input test


    let validTestDataDirectory =
        let thisAssembly = System.Reflection.Assembly.GetExecutingAssembly().Location |> System.IO.FileInfo
        (thisAssembly.Directory.GetDirectories "TestData" |> Seq.exactlyOne).GetDirectories "Valid" |> Seq.exactlyOne

    let validTestData : obj array seq =
        validTestDataDirectory.EnumerateFiles () |> Seq.map (fun fi -> [| fi.Name |])

    [<Theory>]
    [<MemberData("validTestData")>]
    let ``Valid test data can be successfully parsed`` (fileName : string) =
        let input = System.IO.Path.Combine(validTestDataDirectory.FullName, fileName) |> System.IO.File.ReadAllText
        let test = ignore
        assertOneFullParse (Parser.definitions 0) input test
