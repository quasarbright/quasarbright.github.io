module Runner

open FSharp.Text.Lexing
open NUnit.Framework


let private readLines filePath = List.ofSeq (System.IO.File.ReadLines(filePath))

let private getFilePaths (dir : string) : string list = List.ofArray <| System.IO.Directory.GetFiles(dir)

let sourceExtension = "mml"
let outputExtension = "out"
let errorExtension = "err"

let rec private safeZip l1 l2 =
    match (l1, l2) with
        | [], _ | _, [] -> []
        | x1::l1, x2::l2 -> (x1, x2)::(safeZip l1 l2)

let private hasExtension (extension : string) (filename : string) : bool =
    let extLength = extension |> String.length
    let filenameRevChars = filename |> Seq.toList |> List.rev
    let extensionRevChars = extension |> Seq.toList |> List.rev
    if (filename |> String.length) >= (extension |> String.length)
    then
        let filenameRevChars' = filenameRevChars |> List.take extLength
        extensionRevChars = filenameRevChars'
     else
        false

let parse text : (Ast.sourcespan * Ast.sourcespan) Ast.program =
    let lexbuf = LexBuffer<char>.FromString text
    let res = Parser.start Lexer.read lexbuf
    res

let teq a b = Assert.AreEqual(a,b)

type IRunner =
    abstract member Run : ((Ast.sourcespan * Ast.sourcespan) Ast.program -> (string * string))

let testRun (sourceCode : string) (expectedOutput : string) (expectedErrorSubstring : string) (runner : IRunner) =
    let program = sourceCode |> parse
    let (output, errors) = program |> runner.Run
    Assert.AreEqual(expectedOutput, output)
    Assert.That(expectedErrorSubstring, Is.SubsetOf(errors))
