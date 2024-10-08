module Tournament (tournamentSort) where

data Tree x = Empty | Leaf x | Branch x (Tree x) (Tree x) deriving (Show)

createBranch :: (Ord x) => Tree x -> Tree x -> Tree x
createBranch Empty b = b
createBranch a Empty = a
createBranch la@(Leaf a) lb@(Leaf b)
  | a < b = Branch a lb Empty
  | otherwise = Branch b la Empty
createBranch ba@(Branch {}) lb@(Leaf _) = createBranch lb ba -- behaves same as the row below
createBranch la@(Leaf a) bb@(Branch b bl br)
  | a < b = Branch a Empty bb
  | otherwise = Branch b la (createBranch bl br)
createBranch ba@(Branch a al ar) bb@(Branch b bl br)
  | a < b = Branch a (createBranch al ar) bb
  | otherwise = Branch b (createBranch bl br) ba

branchRow :: (Ord x) => [Tree x] -> [Tree x]
branchRow [] = []
branchRow [t] = [t]
branchRow (a : b : bs) = createBranch a b : branchRow bs

createTree :: (Ord x) => [Tree x] -> Tree x
createTree [t] = t
createTree as = createTree $ branchRow as

traverseTree :: (Ord x) => [x] -> Tree x -> [x]
traverseTree xs Empty = xs
traverseTree xs (Leaf x) = x : xs
traverseTree xs (Branch x l r) = x : traverseTree xs (createBranch l r)

tournamentSort :: (Ord a) => [a] -> [a]
tournamentSort [] = []
tournamentSort xs = traverseTree [] $ createTree $ map Leaf xs
