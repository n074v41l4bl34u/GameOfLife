module Counter3


(*********************** very slow code ***********************)

let indexMod k maxK = 
  match k with
  | -1 -> maxK - 1
  | k0 when k0 = maxK -> 0
  | _ -> k

let count3rs (a : _ [,]) x y m n c = 
  //  let i = indexMod x m
  //  let j = indexMod y n  
  let i = 
    match x with
    | -1 -> m - 1
    | x0 when x0 = m -> 0
    | _ -> x
  
  let j = 
    match y with
    | -1 -> n - 1
    | y0 when y0 = n -> 0
    | _ -> y
  
  if a.[i, j] then c + 1
  else c

let rec count3r (a : _ [,]) x y yMin xMax yMax m n x0 y0 c = 
  match c, x, y with
  | 4, _, _ -> 4 //early exit
  | _, cx, _ when cx > xMax -> c
  | _, _, cy when cy > yMax -> count3r a (x + 1) yMin yMin xMax yMax m n x0 y0 c
  | _, cx, cy when cx = x0 && cy = y0 -> count3r a x (y + 1) yMin xMax yMax m n x0 y0 c // skip x=y=0 i.e. middle element
  | _ -> count3r a x (y + 1) yMin xMax yMax m n x0 y0 (count3rs a x y m n c)

let count3 (a : _ [,]) x y m n = 
  let xMin = x - 1
  let xMax = x + 1
  let yMin = y - 1
  let yMax = y + 1
  count3r a xMin yMin yMin xMax yMax m n x y 0

(*****************************************************)