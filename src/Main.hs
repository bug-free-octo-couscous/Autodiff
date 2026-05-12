import Forward_Mode
import Reverse_Mode

f :: Dual -> Dual
f x = x * x + sin x

exampleF :: Node -> IO Node
exampleF x = do
  x2  <- mulN x x
  sx  <- sinN x
  addN x2 sx

-- x = 2에서의 값과 미분값 구하기
-- f(2) = 4 + sin(2) ≒ 4.909
-- f'(2) = 2*2 + cos(2) ≒ 4 - 0.416 = 3.584
main :: IO ()
main = do
    print $ f (Dual 2 1)
    print =<< diff exampleF 2.0
