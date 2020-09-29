prefixSums :: [Int] -> [Int] 
prefixSums ls = 
  let ls' = zip ls [0 .. length ls] in 
  let ls'' = map (\(x,y) -> x * y) ls' in 
  let (_, ls''') = foldl (\(acc, ls''') n -> let acc' = acc + n in (acc', acc' : ls''')) (0, []) ls'' in 
  ls'''
