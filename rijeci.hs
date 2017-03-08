main = interact (myShow . solution . read)

-- After 1 round: B (0,1)
-- If at round n there are x A and y B, then at round n+1 there will be
-- y A and x+y B. This is the Fibonacci sequence.
solution n = (fibs!!(n-1), fibs!!n)
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
myShow (a,b) = show a ++ " " ++ show b ++ "\n"
