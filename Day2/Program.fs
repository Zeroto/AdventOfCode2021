
open System.IO

type Command =
  | Forward of int
  | Down of int
  | Up of int

let parseLine (s: string) =
  let spl = s.Split(' ');
  match spl.[0] with
  | "forward" -> Forward (int spl.[1])
  | "down" -> Down (int spl.[1])
  | "up" -> Up (int spl.[1])
  | _ -> failwith "Invalid input"

let input = File.ReadAllLines("input.txt") |> Array.map parseLine

let part1 () =
  ((0,0), input)
  ||> Array.fold (fun (h,d) cmd ->
    match cmd with
    | Forward x -> (h+x, d)
    | Down x -> (h, d+x)
    | Up x -> (h, d-x)
  )
  |> fun (h,d) -> h*d


let part2 () = 
  ((0, 0, 0), input)
  ||> Array.fold (fun (h, d, a) cmd ->
    match cmd with
    | Forward x -> (h+x, d+a*x, a)
    | Down x -> (h, d, a+x)
    | Up x -> (h, d, a-x)
  )
  |> fun (h,d, _) -> h*d


let sw = System.Diagnostics.Stopwatch.StartNew()
let part1Result = part1 ()
sw.Stop();
printfn "part1: %d, elapsed: %dms" part1Result sw.ElapsedMilliseconds

sw.Restart();
let part2Result = part2 ()
sw.Stop();
printfn "part2: %d, elapsed: %dms" part2Result sw.ElapsedMilliseconds