import Data.Set ( member, insert )
import Data.List.Split ( splitOn )
--

main :: IO ()
main = do
  input1 <- ( parser . lines ) <$> readFile "Day01.input"

  putStr "Part1: "
  print $ part1 input1

  putStr "Part2: "
  print $ part2 input1

--

parser :: [ String ] -> [ Int ]
parser = map $ \x -> ( if head x == '-' then negate else id ) $ read $ tail x

--

part1 :: [ Int ] -> Int
part1 = sum

--

part2 :: [ Int ] -> Int
part2 = go mempty . scanl (+) 0 . cycle
  where go ps ( x:xs ) = if x `member` ps then x else go ( x `insert` ps ) xs
