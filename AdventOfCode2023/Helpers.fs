module AdventOfCode2023.Helpers

let TrimAll (input: array<string>): array<string> =
    input |> Seq.map(fun i -> i.Trim()) |> Array.ofSeq
    
let isNonWhiteSpace (str: string) =
    str.Trim() <> ""
