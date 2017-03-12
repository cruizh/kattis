import Data.List.Split
main :: IO ()
main = interact (output . map solution . input)
  where input = map eachLineIn . tail . lines
        solution = position . getPath
        output = unlines . map eachLineOut . zip [1..]
        eachLineIn = myRead . splitOn "/" . last . words
        myRead [p, q] = (read p, read q)
        eachLineOut (i,n) = unwords (map show [i,n])

data Path = L | R
  deriving (Eq, Show)

getPath :: (Int, Int) -> [Path]
getPath (1,1) = []
getPath (p,q) | p > q = R  : getPath (p-q, q)
              | p < q = L : getPath (p, q-p)

-- position gets a path (bottom-up) and returns the number which corresponds to that path
position :: [Path] -> Int
position xs = (2^l +) . sum . map f . zip [0..] $ xs
 where l = length xs
       f (_, L) = 0
       f (r, R) = 2^r

-- Some helper functions that helped me come up with the solution
-- import Data.Tree
-- import Data.Tree.Pretty

-- leafMap f q (Node x []) = Node x [Node (f x) [], Node (q x) []]
-- leafMap f q (Node x xs) = Node x (map (leafMap f q) xs)

-- treeBase :: Tree (Int, Int)
-- treeBase = Node (1,1) []

-- tree n = (iterate (leafMap downLeft downRight) treeBase) !! n

-- showTree :: Tree (Int, Int) -> IO ()
-- showTree = putStrLn . drawVerticalTree . fmap show

-- downLeft (p,q) = (p,p+q)
-- downRight (p,q) = (p+q,q)

-- check = putStrLn . drawVerticalTree . fmap (show . position . getPath) . tree
-- drawMyTree = putStrLn . drawVerticalTree . fmap show . tree
