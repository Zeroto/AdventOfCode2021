
open System.IO
open System

let input = File.ReadAllLines("input.txt") |> Array.map int

let part1 () =
  ((Int32.MaxValue, 0), input)
  ||> Array.fold (fun (prev, count) item -> 
    if item > prev then
      (item, count + 1)
    else
      (item, count)
  )
  |> snd

let part2 () =
  let windowedSums =
    input 
    |> Array.windowed 3 
    |> Array.map (Array.reduce (+))
    
  ((Int32.MaxValue, 0), windowedSums)
  ||> Array.fold (fun (prev, count) item -> 
    if item > prev then
      (item, count + 1)
    else
      (item, count)
  )
  |> snd

let sw = System.Diagnostics.Stopwatch.StartNew()
let part1Result = part1 ()
sw.Stop();
printfn "part1: %d, elapsed: %dms" part1Result sw.ElapsedMilliseconds

sw.Restart();
let part2Result = part2 ()
sw.Stop();
printfn "part2: %d, elapsed: %dms" part2Result sw.ElapsedMilliseconds