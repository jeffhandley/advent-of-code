open System.IO

type Pos = struct
    val X: int
    val Y: int
    new (x: int, y: int) = { X = x; Y = y }
end

// Create a new position by moving from the previous position.
let movePos (x: int) (y: int) (p: Pos) = Pos(p.X + x, p.Y + y)

// Parse a move sequence into a sequence of move function calls.
// This will convert multi-cell moves into a sequence 1-cell of moves.
let parseMoveSeq (move: string) =
    match move.ToUpper().Split(' ') with
    | [| "U"; count |] -> seq { for c in 1 .. (int count) -> movePos 0 -1 }
    | [| "D"; count |] -> seq { for c in 1 .. (int count) -> movePos 0 +1 }
    | [| "L"; count |] -> seq { for c in 1 .. (int count) -> movePos -1 0 }
    | [| "R"; count |] -> seq { for c in 1 .. (int count) -> movePos +1 0 }
    | _ -> []

// Pull the tail from the head. If they are within 1 cell in any direction,
// including diagonally, then leave the tail where it is. Otherwise, move
// the tail in the direction needed to follow the head, moving up to 1 cell
// on each axis.
let pullPos (h: Pos) (t: Pos) : Pos =
    match (h.X - t.X, h.Y - t.Y) with
    | (x, y) when abs x <= 1 && abs y <= 1 -> t
    | (x, y) -> movePos (x.CompareTo(0)) (y.CompareTo(0)) t

// Create the Head and Tail positions both starting at (0,0).
let mutable H = Pos(0,0)
let mutable T = H

// Create lists for tracking the cells visited by both the Head and Tail.
let mutable visitedH = [H]
let mutable visitedT = [T]

// For each line in the input file, parse the line into a sequence of moves.
// Then execute those moves and track the head and tail visits.
for line in File.ReadAllLines("input.txt") do
    // printfn "MOVE \tHEAD         \tVisits\tTAIL         \tVisits"
    // printfn "%-5s\t(%+5d,%+5d)\t%-6d\t(%+5d,%+5d)\t%-6d" line H.X H.Y (List.distinct visitedH).Length T.X T.Y (List.distinct visitedT).Length

    for move in parseMoveSeq line do
        H <- move H
        T <- pullPos H T

        visitedH <- H :: visitedH
        visitedT <- T :: visitedT

        //printfn "  -->\t(%+5d,%+5d)\t%-6d\t(%+5d,%+5d)\t%-6d" H.X H.Y (List.distinct visitedH).Length T.X T.Y (List.distinct visitedT).Length
    //printfn ""

(*
let xs = List.map (fun (p: Pos) -> p.X) visitedT
let ys = List.map (fun (p: Pos) -> p.Y) visitedT

for y in (List.min ys) .. (List.max ys) do
    for x in (List.min xs) .. (List.max xs) do
        printf (
                if H.X = x && H.Y = y then "H"
                elif T.X = x && T.Y = y then "T"
                elif List.contains (Pos(x, y)) visitedT then "#"
                else "."
        )
    printfn ""
*)

printfn "DONE!\tHEAD         \tVisits\tTAIL         \tVisits"
printfn "     \t(%+5d,%+5d)\t%-6d\t(%+5d,%+5d)\t%-6d" H.X H.Y (List.distinct visitedH).Length T.X T.Y (List.distinct visitedT).Length
