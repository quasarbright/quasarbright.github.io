module UnitTest1

open NUnit.Framework
open Ast
open Runner

let teq a b = Assert.AreEqual(a,b)
let tParse prog expected = teq (prog |> parse |> untag) (untag expected)

[<TestFixture>]
type ParserTests () =
    [<Test>]
    member this.zero() = tParse "0" (EInt(0, ()))
    [<Test>]
    member this.t123() = tParse "123" (EInt(123, ()))
    [<Test>]
    member this.neg1() = tParse "-1" (EInt(-1, ()))
    [<Test>]
    member this.neg0() = tParse "-0" (EInt(0, ()))
    [<Test>]
    member this.f() = tParse "false" (EBool(false, ()))
    [<Test>]
    member this.t() = tParse "true" (EBool(true, ()))
