module GameEngine2

(* 
  clone of GameEngine.fs using GPU matrix transformation concept from:
  http://tomasp.net/blog/accelerator-life-game.aspx/
*)

open Tools
open System

// Configuration of the game
let gridSize, formSize = 512, 512

// Initialization of constant 2D arrays
let shape = [| gridSize; gridSize; |]
let [zero; one; two; three] = [ for f in [0uy; 1uy; 2uy; 3uy] -> Array2D.create gridSize gridSize f ]
//
//// Calculating with 2D float arrays 
//let rotate (a:FPA) (dx:int) dy = PA.Rotate(a, [| dx; dy |]);
//
//// Custom operators - simulating logical operations using floats
//let (&&.) (a:FPA) (b:FPA) = PA.Min(a, b)
//let (||.) (a:FPA) (b:FPA) = PA.Max(a, b)
//let (==.) (a:FPA) (b:FPA) = PA.Cond(PA.CompareEqual(a, b), one, zero)
let (&&.) (a:byte[,]) (b:byte[,]) = Array2D.mapi (fun i j a0 -> Math.Min(a0, b.[i,j])) a// Array.map2 (fun a0 b0 -> Array.map2 (fun a1 b1 -> a1 && b1) a0 b0) a b
let (||.) (a:byte[,]) (b:byte[,]) =  Array2D.mapi (fun i j a0 -> Math.Max(a0, b.[i,j])) a//Array2D.mapi (fun i j -> state.[i,j] + t.[i,j]) Array.map2 (fun a0 b0 -> Array.map2 (fun a1 b1 -> a1 || b1) a0 b0) a b
let (==.) (a:byte[,]) (b:byte[,]) = Array2D.init (Array2D.length1 a) (Array2D.length2 a) (fun i j -> match a.[i,j] = b.[i,j] with |true -> 1uy | _ -> 0uy)// Array2D.mapi (fun i j a0 -> match a0 = b.[i,j] with |true -> 255uy | _ -> 0uy) a//Array2D.mapi (fun i j -> state.[i,j] + t.[i,j]) Array.map2 (fun a0 b0 -> Array.map2 (fun a1 b1 -> a1 = b1) a0 b0) a b
let (++.) (state:byte[,]) (t:byte[,]) = Array2D.mapi (fun i j t0 -> state.[i,j] + t0) t
//
//let nextGeneration (grid:FPA) =
//  // Generate grids shifted by 1 in every direction
//  let hd::tl = 
//    [ for dx in -1 .. 1 do
//        for dy in -1 .. 1 do
//          if dx <> 0 || dy <> 0 then 
//            yield rotate grid dx dy ]
//
//  // Count neighbors of each grid element
//  let sum = tl |> List.fold (+) hd
//  
//  // Implements the Game of Life logic
//  (sum ==. three) ||. ((sum ==. two) &&. grid)
let shiftedValue (a:_ [,]) (x) (y) (xMax:int) (yMax:int) =   
  match x,y with
  | -1,_ -> 0uy
  | _,-1 -> 0uy
  | v,_ when v=xMax -> 0uy
  | _,v when v=yMax -> 0uy
  | _ -> a.[x,y]


let shift (a:_ [,]) (dx:int) (dy:int) =
  let xMax = a.GetLength 0
  let yMax = a.GetLength 1
  Array2D.init xMax yMax (fun x y -> (shiftedValue a (x+dx) (y+dy) xMax yMax))

let shiftArrays (a:_ [,]) : _ [,][] =
  let indexes = 
    [| for dx in -1 .. 1 do
        for dy in -1 .. 1 do
          if dx <> 0 || dy <> 0 then 
            yield (dx,dy) |]
  let arrs = Array.Parallel.init (indexes.Length) (fun i -> shift a (fst indexes.[i]) (snd indexes.[i]) )
  arrs

let countNeighbours (a: _ [,][]) : _[,] =
  let head = a.[0]
  let maxI = (Array2D.length1 head) - 1
  let maxJ = (Array2D.length2 head) - 1
  //let sumArr = tail |> Array.fold (++.) head
  for i in 0..maxI do
    for j in 0..maxJ do
      for k in 1..a.Length-1 do
        Array2D.set head i j (head.[i,j] + a.[k].[i,j])
  head

let rule world neighboursInTheWorld =
  let newWorld = (neighboursInTheWorld ==. three) ||. ((neighboursInTheWorld ==. two) &&. world)
  newWorld

let nextGeneration (world:_[,]) =
  // Generate grids shifted by 1 in every direction
  let worldPlanes = shiftArrays world

  // Count neighbors of each grid element
  let sum = worldPlanes.[1..] |> Array.fold (++.) (worldPlanes.[0])
  
  // Implements the Game of Life logic
  let newWorld = (sum ==. three) ||. ((sum ==. two) &&. world)
  newWorld

open System.Threading.Tasks
open System.Windows
open System.Windows.Media.Imaging



let mainLoop () =
//do
  let rand = System.Random()
  let w = 16
  let n = 256
  let formSize = float (2*n)
  let worldInit x = Array2D.init x x (fun _ _ -> rand.Next 2 |> byte) |> ref
  let worlds = Array.init w (fun _ -> worldInit n)
  let image = Controls.Image(Stretch=Media.Stretch.Uniform)
  let format = Media.PixelFormats.Gray8
  //let avgUpdateDuration = 0
  let updatesCount = 0 |> ref
  //let pixels = Array.create (n*n) 0uy
  let update _ =
#if DEBUG
    let sw1, sw2 = System.Diagnostics.Stopwatch(),System.Diagnostics.Stopwatch()
    sw1.Start()
#endif    
    Parallel.For(0,w-1,      
      (fun k ->
        let worldk = !(worlds.[k])
        worlds.[k] := 
          worldk
//          |> nextGeneration
          |> shiftArrays
          |> countNeighbours  
          |> rule worldk
      )
    ) |> ignore
#if DEBUG
    sw1.Stop()
    sw2.Start()
#endif
    let pixels = 
      worlds
      |> pixelsOfWorlds
      |> totalBrightness simpleAverage 
//    for i in 0..pixels.Length do
//      printf "%d" pixels.[i]
      //let pix = gameBrightness !games.[i]
//      for x in 0..n-1 do
//        for y in 0..n-1 do
//          pixels.[x+y*n] <- pixels.[x+y*n] + (pixelBrightness (!games.[i]) x y) / (byte k) // if (!game).[x, y] then 255uy else 0uy
    image.Source <-
      BitmapSource.Create(n, n, 1.0, 1.0, format, null, pixels, n)   
#if DEBUG
    sw2.Stop()
    updatesCount := !updatesCount + 1
    if !updatesCount % 10 = 0 then 
      let total = (sw1.Elapsed.TotalMilliseconds + sw2.Elapsed.TotalMilliseconds)
      printfn "%.2f + %.2f = %.2f (%.2f fps)" sw1.Elapsed.TotalMilliseconds sw2.Elapsed.TotalMilliseconds total (1000.0/total)
#endif
  Media.CompositionTarget.Rendering.Add update
  Window(Content = image, Title = "Game of Life", Width=formSize, Height=formSize)
  |> (Application()).Run |> ignore