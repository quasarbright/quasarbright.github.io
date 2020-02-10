// Learn more about F# at http://fsharp.org

open System



[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    printfn "yo"
    let input = Console.ReadLine() in
    printfn "%s" input
    0 // return an integer exit code
