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
    (Dual u u') / (Dual v v') = Dual (u / v) ((u' * v - u * v') / (v * v))
    fromRational r = Dual (fromRational r) 0

instance Floating Dual where
    pi = Dual pi 0
    exp (Dual u u') = Dual (exp u) (u' * exp u)
    log (Dual u u') = Dual (log u) (u' / u)
    sin (Dual u u') = Dual (sin u) (u' * cos u)
    cos (Dual u u') = Dual (cos u) (-u' * sin u)
    
    -- Inverse Trig: d/dx asin(x) = 1 / sqrt(1 - x^2)
    asin (Dual u u') = Dual (asin u) (u' / sqrt (1 - u*u))
    acos (Dual u u') = Dual (acos u) (-u' / sqrt (1 - u*u))
    atan (Dual u u') = Dual (atan u) (u' / (1 + u*u))
    
    -- Hyperbolic: d/dx sinh(x) = cosh(x)
    sinh (Dual u u') = Dual (sinh u) (u' * cosh u)
    cosh (Dual u u') = Dual (cosh u) (u' * sinh u)
    
    -- Inverse Hyperbolic
    asinh (Dual u u') = Dual (asinh u) (u' / sqrt (u*u + 1))
    acosh (Dual u u') = Dual (acosh u) (u' / sqrt (u*u - 1))
    atanh (Dual u u') = Dual (atanh u) (u' / (1 - u*u))

    -- f(x) = x^2 + sin(x)
f :: Dual -> Dual
f x = x * x + sin x

-- x = 2에서의 값과 미분값 구하기
-- f(2) = 4 + sin(2) ≒ 4.909
-- f'(2) = 2*2 + cos(2) ≒ 4 - 0.416 = 3.584
main :: IO ()
main = print $ f (Dual 2 1)
-- 출력 결과: Dual 4.909297426825682 3.5838531634528574