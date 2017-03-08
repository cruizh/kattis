main = interact (show . area . read)

area :: Double -> Double
area = (4*) . sqrt
