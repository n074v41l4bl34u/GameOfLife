module Tools

let createArrArr (size:int) (value) = Array.create size (Array.create size value)

(************************************************)

let simpleAverage (array: byte []) =
  let elemCount = array.Length
  let avg = 
    array
    |> Array.map int //convert byte -> int 
    |> Array.sum
    //|> fun x _ -> x / elemCount 
  avg / elemCount
  |> byte

let transposeArray (array:_ [][]) =
  let k = array.Length
  let dim1 = array.[0].Length
  let arrT = Array.Parallel.init dim1 (fun i -> array.[0..k-1].[i]) 
  arrT

let transposeArray2 (matrix:_ [][]) =
    if matrix.Length = 0 then failwith "Invalid matrix"  
    Array.Parallel.init matrix.[0].Length (fun i -> 
        Array.init matrix.Length (fun j -> 
            matrix.[j].[i]))


let totalBrightness (sumFunction: byte [] -> byte) (array: byte [][]) =
  let arrayT = transposeArray2 array
  let len = arrayT.Length // should equal number of pixels on the screen
  //let pix = Array.create len 0uy
  let pix = Array.Parallel.init len (fun i -> 
              (arrayT.[i]) 
              |> sumFunction 
            )
//  Parallel.For(0, len, 
//    (fun i -> 
//      let s = 
//        (arrayT.[i]) 
//        |> sumFunction 
//      pix.[i] <- s
//    )
//  )
//  |> ignore
  pix
//  let pixels = a |> Array.Parallel.mapi (fun i x -> (integrateBrightness (a.[i]) Array.sum))
//  pixels

(************************************************)

let pixelBrightness (x: bool) =
  if x then 255uy else 0uy

let calculateIndex i x =
  let a = i / x
  let b = i-a*x
  a,b

let convertAllBoolsToBytes (worlds: bool[,] ref []) : byte[][] = 
  let w = worlds.Length
  let x = (!worlds.[0]).GetLength 0
  let y = (!worlds.[0]).GetLength 1
  let result = 
    Array.Parallel.init w //
      (fun k -> 
        Array.init (x*y) 
          (fun i -> 
            let a,b = calculateIndex i x
            let world = !(worlds.[k])
            pixelBrightness world.[a,b]
          )
      )
  result

let convertAllBoolsToBytes2 (worlds: bool[,] []) : byte[][] = 
  let w = worlds.Length
  let x = Array2D.length1 (worlds.[0])
  let y = Array2D.length2 (worlds.[0])
  let result = 
    Array.Parallel.init w //
      (fun k -> 
        let world = (worlds.[k])
        Array.init (x*y) 
          (fun i -> 
            let a,b = calculateIndex i x
            pixelBrightness world.[a,b]
          )
      )
  result

/// returns: pixels[] of each world k i.e. pixelsOfWorlds.[k]
let pixelsOfWorlds (worlds: _[,] ref []) : byte[][] = 
  let w = worlds.Length
  let x = (!worlds.[0]).GetLength 0
  let y = (!worlds.[0]).GetLength 1
  let result = 
    Array.Parallel.init w //
      (fun k -> 
        Array.init (x*y) 
          (fun i -> 
            let a,b = calculateIndex i x
            let world = !(worlds.[k])
            match world.[a,b] with | 0uy -> 0uy | _ -> 255uy
          )
      )
  result

(************************************************)

type 'T ``[,,]`` with
  member x.GetSlice(rowStart: int option, rowFinish : int option,
                    colStart: int option, colFinish : int option,
                    depthStart: int option, depthFinish:int option) =
    let rowStart = match rowStart with
                    | Some(v) -> v
                    | None -> 0
    let rowFinish = match rowFinish with
                    | Some(v) -> v
                    | None -> (Array3D.length1 x) - 1
    let colStart = match colStart with
                    | Some(v) -> v
                    | None -> 0
    let colFinish = match colFinish with
                    | Some(v) -> v
                    | None -> (Array3D.length2 x) - 1           
    let depthStart = match depthStart with
                      | Some(v) -> v
                      | None -> 0
    let depthFinish = match depthFinish with
                      | Some(v) -> v
                      | None -> (Array3D.length3 x) - 1 
    Array3D.init (rowFinish-rowStart) (colFinish-colStart) (depthFinish-depthStart) (fun i j k -> Array3D.get x i j k)

  member x.GetSlice(rowStart: int option, rowFinish : int option,
                    colStart: int option, colFinish : int option,
                    depth:int) =
    let rowStart = match rowStart with
                    | Some(v) -> v
                    | None -> 0
    let rowFinish = match rowFinish with
                    | Some(v) -> v
                    | None -> (Array3D.length1 x) - 1
    let colStart = match colStart with
                    | Some(v) -> v
                    | None -> 0
    let colFinish = match colFinish with
                    | Some(v) -> v
                    | None -> (Array3D.length2 x) - 1
    Array2D.init (rowFinish-rowStart) (colFinish-colStart) (fun i j -> Array3D.get x i j depth)