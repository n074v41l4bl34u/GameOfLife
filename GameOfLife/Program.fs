// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

[<System.STAThread>]
[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    GameEngine.mainLoop ()
    //GameEngine2.mainLoop ()
    0 // return an integer exit code
