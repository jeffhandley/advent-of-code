open System.IO

let moves = File.ReadAllLines("input.txt")

moves
|> Array.iter (fun move -> printfn "%s" move)
