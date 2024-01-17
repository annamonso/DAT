inc :: Double -> Double
inc i = i + 1


quadrat :: Double -> Double
quadrat i = i * i

mult :: Double -> Double -> Double
mult a t = a * t


triple :: Double -> Double
triple  = mult 3


distEuclid :: Double -> Double -> Double -> Double -> Double
distEuclid p1X p1Y p2X p2Y = sqrt(quadrat(p2X-p1X) + quadrat(p2Y-p1Y))

distEuclidori ::  Double -> Double -> Double
distEuclidori = distEuclid 0 0

factorial_1 :: Int -> Int
factorial_1 n = if n==0 then 1 else (n*factorial_1(n-1))


sumatori :: Int -> Int
sumatori n = if n==0 then 0 else (n+sumatori(n-1))


inversa :: Float -> Float
inversa k = 1/k

inversaDouble :: Double -> Double
inversaDouble k = 1/k


sumainversa :: Double -> Double
sumainversa n = if n==1 then 1 else (inversaDouble(n)+sumainversa(n-1))

quadratInc :: Double -> Double
quadratInc = quadrat . inc


incQuadrat :: Double -> Double
incQuadrat = inc . quadrat


sumainversaQuadrat :: Double -> Double
sumainversaQuadrat n = if n==1 then 1 else ((inversaDouble . quadrat) n) + sumainversaQuadrat (n-1)


factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)


sumat :: Int -> Int
sumat 0 = 0
sumat n = n + sumatori(n-1)

