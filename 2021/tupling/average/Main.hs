module Average where 

import Text.Printf ( printf ) 

simpleAverage :: (Ord a, Num a, Fractional a) => [a] -> a
simpleAverage xs =
    let s = sum xs in 
    let l = length xs in  
    s / l
  where 
    length [] = 0 
    length (_:t) = 1 + length t 
    sum [] = 0 
    sum (h:t) = h + sum t  

smartAverage :: (Ord a, Num a, Fractional a) => [a] -> a
smartAverage xs = 
    let (s, l) = average xs in
    s / l  
  where 
    average [] = (0, 0) 
    average (h:t) = 
      let (s, l) = average t in 
      (h + s, l + 1)
      
run :: (Ord a, Num a, Fractional a, Show a) => [a] -> IO () 
run xs = do 
    go simpleAverage xs 
    go smartAverage xs 
  where 
    go average xs = do  
      let a = average xs
      putStrLn $ printf "Input:\t%s\nAverage:\t%s\n\n" (show xs) (show a)

main :: IO () 
main = do 
  run [1,2,3,4,5]
  run [] 
  run [13, 42, 7, 1, -1]
