open System.IO

type Pos = struct
    val X: int
    val Y: int
    new (x: int, y: int) = { X = x; Y = y }
end

let movePos (x: int) (y: int) (p: Pos) = Pos(p.X + x, p.Y + y)
let pullPos (h: Pos) (t: Pos) : Pos =
    let x = h.X - t.X
    let y = h.Y - t.Y
    
    if   y = 0 && x = -2 then movePos -1  0 t
    elif y = 0 && x = +2 then movePos +1  0 t
    elif x = 0 && y = -2 then movePos  0 -1 t
    elif x = 0 && y = +2 then movePos  0 +1 t
    elif x < 0 && y <  0 then movePos -1 -1 t
    elif x < 0 && y >  0 then movePos -1 +1 t
    elif x > 0 && x <  0 then movePos +1 -1 t
    elif x > 0 && y >  0 then movePos +1 +1 t
    else t

let parseMoveSeq (move: string) =
    match move.ToUpper().Split(' ') with
    | [| "U"; count |] -> seq { for c in (int count) .. -1 .. 1 -> movePos 0 -c }
    | [| "D"; count |] -> seq { for c in (int count) .. -1 .. 1 -> movePos 0 +c }
    | [| "L"; count |] -> seq { for c in (int count) .. -1 .. 1 -> movePos -c 0 }
    | [| "R"; count |] -> seq { for c in (int count) .. -1 .. 1 -> movePos +c 0 }
    | _ -> []

let mutable H = Pos(0,0)
let mutable T = H
let mutable visitedH = [H]
let mutable visitedT = [T]

let moves = File.ReadAllLines("input.txt")
let moveSeq = seq { for move in moves do yield! parseMoveSeq move }

for move in (Seq.take 10 moveSeq) do
    H <- move H
    T <- pullPos H T

    visitedH <- H :: visitedH
    visitedT <- T :: visitedT

    printfn "H: (%d,%d) [%d visits] T: (%d,%d) [%d visits]" H.X H.Y (List.distinct visitedH).Length T.X T.Y (List.distinct visitedT).Length
