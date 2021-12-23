open System.IO
open System

let parseInput (s: string) =
  s.Split(',')
  |> Array.map int

let input =
  File.ReadAllText("input.txt")
  |> parseInput
  |> Array.toList

let part1 () =
  (input, [0..79])
  ||> List.fold (fun fish i ->
    let createFishCount = fish |> List.filter ((=) 0) |> List.length
    let newFish =
      fish
      |> List.map (fun f -> if f = 0 then 6 else f-1)
    let newfishes = Array.create createFishCount 8 |> Array.toList
    newFish @ newfishes
  )
  |> List.length

let part2 () =
  let buckets = input |> List.countBy id |> Map.ofList |> Map.map (fun _ i -> (bigint i, bigint 0))
  (buckets, [0..255])
  ||> List.fold (fun buckets i ->
    // printfn "%d: %A" i (buckets)
    let index = i % 7
    let (toAdd, newCount) = buckets |> Map.tryFind index |> Option.defaultValue (bigint 0, bigint 0)
    let buckets = buckets |> Map.add index (toAdd+newCount,  bigint 0)
    let addIndex = (index+2) % 7
    let (nextBucketToAdd, nextBucketNewCount) = buckets |> Map.tryFind addIndex |> Option.defaultValue (bigint 0, bigint 0)
    buckets |> Map.add addIndex (nextBucketToAdd, nextBucketNewCount+toAdd)
  )
  |> Map.toList
  |> List.map (snd >> (fun (a,c) -> a+c))
  |> List.sum

let sw = System.Diagnostics.Stopwatch.StartNew()
let part1Result = part1 ()
sw.Stop();
printfn "part1: %A, elapsed: %dms" part1Result sw.ElapsedMilliseconds

sw.Restart();
let part2Result = part2 ()
sw.Stop();
printfn "part2: %A, elapsed: %dms" part2Result sw.ElapsedMilliseconds