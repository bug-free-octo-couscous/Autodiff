data Dual = Dual Double Double deriving (Show, Eq)

instance Num Dual where
    -- (u + u'ε) + (v + v'ε) = (u+v) + (u'+v')ε
    (Dual u u') + (Dual v v') = Dual (u + v) (u' + v')
    
    -- (u + u'ε) * (v + v'ε) = uv + (uv' + u'v)ε  <- 곱의 미분법!
    (Dual u u') * (Dual v v') = Dual (u * v) (u * v' + u' * v)
    
    fromInteger n = Dual (fromInteger n) 0
    abs (Dual u u') = Dual (abs u) (u' * signum u)
    signum (Dual u _) = Dual (signum u) 0
    negate (Dual u u') = Dual (negate u) (negate u')

instance Fractional Dual where
    -- 몫의 미분법
    (Dual u u') / (Dual v v') = Dual (u / v) ((u' * v - u * v') / (v * v))

instance Floating Dual where
    -- sin(u + u'ε) = sin(u) + u'cos(u)ε
    sin (Dual u u') = Dual (sin u) (u' * cos u)
    cos (Dual u u') = Dual (cos u) (-u' * sin u)
    exp (Dual u u') = Dual (exp u) (u' * exp u)

    -- f(x) = x^2 + sin(x)
f :: Dual -> Dual
f x = x * x + sin x

-- x = 2에서의 값과 미분값 구하기
-- f(2) = 4 + sin(2) ≒ 4.909
-- f'(2) = 2*2 + cos(2) ≒ 4 - 0.416 = 3.584
main :: IO ()
main = print $ f (Dual 2 1)
-- 출력 결과: Dual 4.909297426825682 3.5838531634528574