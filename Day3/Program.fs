
open System.IO
open System

let input = File.ReadAllLines("input.txt")
let length = input.[0].Length

let parseValue (c:char) =
  match c with
  | '0' -> 0
  | '1' -> 1
  | _ -> failwith "Invalid input"

let bitsToNumber (bits: int seq) =
  bits
  |> Seq.mapi (fun i s -> s <<< (length-1-i))
  |> Seq.sum

let invert length (n: int) =
  ~~~n &&& ((Math.Pow (2, length) |> int) - 1)

let part1 () =
  (Array.zeroCreate<int>(length) |> Seq.ofArray, input)
  ||> Array.fold (fun s item ->
    item
    |> Seq.zip s
    |> Seq.map (fun (state, value) -> state + parseValue value)
  )
  |> Seq.map (fun s -> if s >= input.Length/2 then 1 else 0)
  |> bitsToNumber
  |> fun s -> s * (invert length s)

let countColumn (input: char array array) =
  input
  |> Array.map (Array.head >> parseValue)
  |> Array.sum

// ugly solution but can't be bothered to make it nicer
let part2 () = 
  ((input |> Array.map (fun s -> s.ToCharArray()), input |> Array.map (fun s -> s.ToCharArray()), []), seq {1..(input.[0].Length)})
  ||> Seq.fold (fun (oxygenInput, co2Input, output) _ ->
    printf "oxy: %A " (oxygenInput)
    printfn "co2: %A" (co2Input)
    let oxygen =
      if oxygenInput.Length = 1 then
        parseValue (oxygenInput.[0].[0])
      else
        let oxygenCount = countColumn oxygenInput
        if oxygenCount >= oxygenInput.Length - oxygenCount then 1 else 0
    let co2 =
      if co2Input.Length = 1 then
        parseValue (co2Input.[0].[0])
      else
        let co2Count = countColumn co2Input
        if co2Count >= co2Input.Length - co2Count then 0 else 1
    let newOxygenInput = (if oxygenInput.Length > 1 then oxygenInput |> Array.filter (fun s -> parseValue s.[0] = oxygen) else oxygenInput) |> Array.map (Array.tail) 
    let newCo2Input = (if co2Input.Length > 1 then co2Input |> Array.filter (fun s -> parseValue s.[0] = co2) else co2Input) |> Array.map (Array.tail)
    (newOxygenInput, newCo2Input, output @ [(oxygen, co2)])
  )
  |> fun (_, _, bits) ->
    let oxygen = bits |> List.map fst |> bitsToNumber
    let co2 = bits |> List.map snd |> bitsToNumber
    bits, oxygen, co2, oxygen * co2



let sw = System.Diagnostics.Stopwatch.StartNew()
let part1Result = part1 ()
sw.Stop();
printfn "part1: %A, elapsed: %dms" part1Result sw.ElapsedMilliseconds

sw.Restart();
let part2Result = part2 ()
sw.Stop();
printfn "part2: %A, elapsed: %dms" part2Result sw.ElapsedMilliseconds