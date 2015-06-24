module Counter2


let count2 (a : _ [,]) x y m n = 
  let xMin = x - 1
  let xMax = x + 1
  let yMin = y - 1
  let yMax = y + 1
  let mutable c = 0
  for x in xMin..xMax do
    for y in yMin..yMax do
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
      
      if a.[i, j] then c <- c + 1
  if a.[x, y] then c - 1
  else c