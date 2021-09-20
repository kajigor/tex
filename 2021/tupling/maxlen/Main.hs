module MaxLen where 

import Text.Printf ( printf ) 

simpleMaxLen :: (Ord a, Num a) => [a] -> (a, Int)
simpleMaxLen xs =
    let m = max xs in 
    let l = length xs in  
    (m, l)
  where 
    length [] = 0 
    length (_:t) = 1 + length t 
    max [] = 0 
    max (h:t) = 
      let m = max t in 
      if m > h then m else h       

smartMaxLen :: (Ord a, Num a) => [a] -> (a, Int)
smartMaxLen [] = (0, 0)
smartMaxLen (h:t) = 
  let (m, l) = smartMaxLen t in 
  (if m > h then m else h, l + 1)   

run :: (Ord a, Num a, Show a) => [a] -> IO () 
run xs = do 
    go simpleMaxLen xs 
    go smartMaxLen xs 
  where 
    go maxLen xs = do  
      let (max, len) = maxLen xs
      putStrLn $ printf "Input:\t%s\nLength\t%s\nMax:\t%s\n\n" (show xs) (show len) (show max)


main :: IO () 
main = do 
  run [1,2,3,4,5]
  run [] 
  run [13, 42, 7, 1, -1]
