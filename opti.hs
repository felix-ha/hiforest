f :: Float -> Float
f x = x * x

f' :: (Float -> Float) -> Float -> Float -> Float 
f' f x h = (f (x+h) - f x) / h 

nextStep :: (Float -> Float) -> Float -> Int -> (Float, Int)
nextStep f x i = 
  let 
    h = 1e-5
    alpha = 0.1
    diff = f' f x h
    xn = x - alpha * diff
  in (xn, i - 1) 

opti :: (Float -> Float) -> Float -> Int -> (Float, Int)
opti f x 0 = (x, 0)
opti f x numSteps = 
  let 
  (xn, i) = nextStep f x numSteps
  in opti f xn i

main = print $
    opti f 4 25
    

