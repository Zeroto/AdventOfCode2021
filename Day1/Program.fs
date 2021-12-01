
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
  ((Int32.MaxValue, 0), input |> Array.windowed 3 |> Array.map (Array.reduce (+)))
  ||> Array.fold (fun (prev, count) item -> 
    if item > prev then
      (item, count + 1)
    else
      (item, count)
  )
  |> snd

let part1Result = part1 ()
printfn "part1: %d" part1Result

let part2Result = part2 ()
printfn "part2: %d" part2Result