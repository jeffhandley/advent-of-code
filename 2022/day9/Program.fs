// https://adventofcode.com/2022/day/9
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

// Pull from the previous knot in the rope. If they are within 1 cell in
// any direction, including diagonally, then no movement is needed.
// Otherwise, move to follow the head, moving 0 or 1 position on each axis.
let pullPos (h: Pos) (t: Pos) : Pos =
    match (h.X - t.X, h.Y - t.Y) with
    | (x,y) when abs x <= 1 && abs y <= 1 -> t
    | (x,y) -> movePos (x.CompareTo(0)) (y.CompareTo(0)) t

let processRopeMoves (knotCount: int) (verbosity: int) =
    // Create the rope with all knot positions initialized to (0, 0).
    let mutable knots = Array.create knotCount (Pos(0, 0))

    // Track the visits for all knots in the rope.
    let mutable visits = Array.create knotCount [Pos (0, 0)]

    // For each line in the input file, parse the line into a sequence of moves.
    // Then execute those moves and track the visits of every knot in the rope.
    // https://adventofcode.com/2022/day/9/input
    for line in File.ReadAllLines("input.txt") do
        let (H, T) = (Array.head knots, Array.last knots)

        if verbosity > 1 then
            printfn "MOVE \tHEAD         \tVisits\tTAIL         \tVisits"
            printfn "%-5s\t(%+5d,%+5d)\t%-6d\t(%+5d,%+5d)\t%-6d" line H.X H.Y (List.distinct (Array.head visits)).Length T.X T.Y (List.distinct (Array.last visits)).Length

        for move in parseMoveSeq line do
            Array.set knots 0 (move knots[0])
            Array.set visits 0 (knots[0] :: visits[0])

            for k in 1 .. (Array.length knots - 1) do
                Array.set knots k (pullPos knots[k - 1] knots[k])
                Array.set visits k (knots[k] :: visits[k])

            if verbosity > 1 then
                printfn "  -->\t(%+5d,%+5d)\t%-6d\t(%+5d,%+5d)\t%-6d" H.X H.Y (List.distinct (Array.head visits)).Length T.X T.Y (List.distinct (Array.last visits)).Length

        if verbosity > 1 then
            printfn ""

    let (H, T) = (Array.head knots, Array.last knots)

    if verbosity > 0 then
        let tailVisits = (Array.last visits)
        let xs = List.map (fun (p: Pos) -> p.X) tailVisits
        let ys = List.map (fun (p: Pos) -> p.Y) tailVisits

        for y in (List.min ys) .. (List.max ys) do
            for x in (List.min xs) .. (List.max xs) do
                printf (
                    if   H.X = x && H.Y = y then "H"
                    elif T.X = x && T.Y = y then "T"
                    elif   0 = x &&   0 = y then "s"
                    elif List.contains (Pos(x, y)) tailVisits then "#"
                    else "."
                )
            printfn ""

        printfn ""
        printfn "DONE!\tHEAD         \tVisits\tTAIL         \tVisits"
        printfn "     \t(%+5d,%+5d)\t%-6d\t(%+5d,%+5d)\t%-6d" H.X H.Y (List.distinct (Array.head visits)).Length T.X T.Y (List.distinct (Array.last visits)).Length
        printfn ""

    visits

let partOneVisits = processRopeMoves  2 1
let partTwoVisits = processRopeMoves 10 1

printfn "[PART ONE ANSWER] %d" ((List.distinct (Array.last partOneVisits)).Length)
printfn "[PART TWO ANSWER] %d" ((List.distinct (Array.last partTwoVisits)).Length)
