module Counter4

(*********************** worked bu now seem o have a bug in c-variable accounting ***********************)
let count4 (a : _ [,]) x y m n = 
  let xMin = x - 1
  let xMax = x + 1
  let yMin = y - 1
  let yMax = y + 1
  let mutable c = 0
  let mutable cx = xMin
  let mutable cy = yMin
  while c < 4 && cx < xMax do
    let i = 
      match cx with
      | -1 -> m - 1
      | x0 when x0 = m -> 0
      | _ -> cx
    
    let j = 
      match cy with
      | -1 -> n - 1
      | y0 when y0 = n -> 0
      | _ -> cy
    
    if a.[i, j] then 
      c <- c + 1
      if i = x && j = y then c <- c - 1
    if cy = yMax then 
      cy <- yMin
      cx <- cx + 1
    else cy <- cy + 1
  //  if c0 then c - 1
  //  else c
  c
