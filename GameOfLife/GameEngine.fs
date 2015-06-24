module GameEngine

(*
  clone of http://fsharpnews.blogspot.com/2010/02/john-conways-game-of-life-in-32-lines.html
  with multiple independent world planes
*)

open Microsoft.FSharp.Collections
open System.Threading.Tasks
open Tools

// Configuration of the game
let gridSize, formSize = 256, 512



open Counter2

let rule (a : _ [,], m, n) x y = 
  match a.[x, y], count2 a x y m n with
  | true, (2 | 3) | false, 3 -> true
  | _ -> false

open System.Windows
open System.Windows.Media.Imaging

let mainLoop() = 
  //do
  let rand = System.Random()
  let w = 16 // world planes count
  let n = gridSize
  let formSize = float (2 * n)
  let worldInit x = Array2D.init x x (fun _ _ -> rand.Next 2 = 0) // |> ref
  let worlds = Array.init w (fun _ -> worldInit n)
  let image = Controls.Image(Stretch = Media.Stretch.Uniform)
  let format = Media.PixelFormats.Gray8
  let updatesCount = 0 |> ref
  
  let update _ =
#if DEBUG 
    let sw1, sw2 = System.Diagnostics.Stopwatch(), System.Diagnostics.Stopwatch()
    sw1.Start()
#endif
    
    Parallel.For(0, w - 1, 
                 (fun i -> 
                 let worldi = worlds.[i]
                 
                 let g = 
                   (worldi, (Array2D.length1 worldi), (Array2D.length2 worldi))
                   |> rule
                   |> Array2D.init n n
                 worlds.[i] <- g))
    //games.SetValue(g, i)))
    //Array.set games i g))
    |> ignore
#if DEBUG
    sw1.Stop()
    sw2.Start()
#endif
    
    let allBytes = worlds |> convertAllBoolsToBytes2
    let pixels = allBytes |> totalBrightness simpleAverage
    image.Source <- BitmapSource.Create(n, n, 1.0, 1.0, format, null, pixels, n)
#if DEBUG
    sw2.Stop()
    updatesCount := !updatesCount + 1
    if !updatesCount % 10 = 0 then 
      let total = (sw1.Elapsed.TotalMilliseconds + sw2.Elapsed.TotalMilliseconds)
      printfn "%.2f + %.2f = %.2f (%.2f fps)" sw1.Elapsed.TotalMilliseconds sw2.Elapsed.TotalMilliseconds total 
        (1000.0 / total)
#endif
    
  Media.CompositionTarget.Rendering.Add update
  Window(Content = image, Title = "Game of Life", Width = formSize, Height = formSize)
  |> (Application()).Run
  |> ignore
