open System.IO
open System

type PlayBoard = (int * bool) array array

let input = File.ReadAllLines "input.txt"
let parseBingoNumbers (l: string) =
  l.Split(",")
  |> Array.map int
  |> Array.toList

let parseBoard (b: string array): PlayBoard =
  let parseLine (l: string) = l.Split(" ", StringSplitOptions.RemoveEmptyEntries) |> Array.map (fun x -> int x, false)
  b |> Array.map parseLine

let bingoNumbers = parseBingoNumbers (input.[0])
let boards = 
  input 
  |> Array.skip 2 
  |> Array.chunkBySize 6 
  |> Array.map (Array.take 5 >> parseBoard)

let isWinningBoard (board: PlayBoard) =
  let check a =
    a |> Array.exists (fun (_, b) -> not b) |> not

  let hasWinningRow =
    board
    |> Array.map check
    |> Array.contains true

  let hasWinningColumns =
    board
    |> Array.transpose
    |> Array.map check
    |> Array.contains true

  hasWinningRow || hasWinningColumns

let getWinningBoard (boards: PlayBoard array) =
  boards
  |> Array.tryFind (isWinningBoard)

let getWinningBoards (boards: PlayBoard array) =
  boards
  |> Array.filter (isWinningBoard)

let play number (board: PlayBoard): PlayBoard =
  board
  |> Array.map (Array.map (fun (s, b) -> (s, b || s = number)))

let playBoards (boards: PlayBoard array) number: PlayBoard array =
  boards
  |> Array.map (play number)

let calcScore (winningBoard: PlayBoard) number =
  winningBoard
  |> Array.collect (Array.filter (snd >> not))
  |> Array.map fst
  |> Array.sum
  |> (*) number

let part1 () =
  let rec solve (boards: PlayBoard array) (numbers: int list) =
    match numbers with
    | x :: xs ->
      let newBoards = playBoards boards x
      match getWinningBoard newBoards with
      | Some b -> (b, x)
      | None -> solve newBoards xs
    | [] -> failwith "No winning board"

  let (winningBoard, number) = solve boards bingoNumbers

  calcScore winningBoard number

let part2 () =
  let rec solve lastWinningBoard (boards: PlayBoard array) (numbers: int list) =
    match numbers with
    | x :: xs ->
      let newBoards = playBoards boards x
      match getWinningBoards newBoards with
      | [|b|] -> solve (Some (b, x)) (newBoards |> Array.except [b]) xs // Single winning board
      | x -> solve lastWinningBoard (newBoards |> Array.except x) xs // 0 or more winning boards
    | [] -> lastWinningBoard

  let (winningBoard, number) = 
    match solve None boards bingoNumbers with
    | Some x -> x
    | None -> failwith "No winning board found"

  printfn "%A %A" winningBoard number
  calcScore winningBoard number

let sw = System.Diagnostics.Stopwatch.StartNew()
let part1Result = part1 ()
sw.Stop();
printfn "part1: %A, elapsed: %dms" part1Result sw.ElapsedMilliseconds

sw.Restart();
let part2Result = part2 ()
sw.Stop();
printfn "part2: %A, elapsed: %dms" part2Result sw.ElapsedMilliseconds