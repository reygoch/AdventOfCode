import Data.Map ( Map, alter )
import Data.Monoid ( Sum (..) )
import Data.Bool ( bool )
import Data.Foldable ( fold )
import Data.Bifunctor ( Bifunctor, bimap )
--

main :: IO ()
main = do
  input <- lines <$> readFile "Day02.input"

  putStr "Part1: "
  print $ part1 input

  putStr "Part2: "
  print $ part2 input

--

part1 :: [ String ] -> Word
part1 = calcHash . fmap ( repeatCnt . repeats )

--

part2 :: [ String ] -> String
part2 ss = head $
  [ fmap fst $ filter ( uncurry (==) ) $ zip s1 s2
  | s1 <- ss
  , s2 <- ss
  , diff s1 s2 == 1
  ]

--

repeats :: String -> Map Char Word
repeats = foldr ( alter $ Just . maybe 1 (+1) ) mempty

repeatCnt :: Map Char Word -> ( Word, Word )
repeatCnt = bid ( min 1 ) . foldr ( \n -> bimap ( go 2 n ) ( go 3 n ) ) ( 0, 0 )
  where go n = bool id (+1) . (==n)

calcHash :: [ ( Word, Word ) ] -> Word
calcHash = uncurry (*) . bid getSum . fold . fmap ( bid Sum )

--

diff :: String -> String -> Word
diff s = sum . zipWith ( \c1 c2 -> bool 1 0 ( c1 == c2 )) s

--

bid :: Bifunctor p => ( a -> b ) -> p a a -> p b b
bid f = bimap f f
