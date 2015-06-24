module Counter


let count (a : _ [,]) x y m n = 
  let mutable c = 0
  for x in x - 1..x + 1 do
    for y in y - 1..y + 1 do
      if x >= 0 && x < m && y >= 0 && y < n && a.[x, y] then c <- c + 1
  if a.[x, y] then c - 1
  else c