import Data.Char
import Data.Set ( fromList )
--

main :: IO ()
main = do
  input <- ( ( !! 0 ) . lines ) <$> readFile "Day05.input"

  putStr "Part1: "
  print $ extractLength $ collapse $ cursor input

  putStr "Part2: "
  print
    $ minimum
    $ fmap ( extractLength . collapse . cursor )
    $ fmap ( $input )
    $ fmap ( \ c -> filter ( \ c' -> toLower c' /= c ) ) ['a'..'z']

--

data Cursor a = Cursor
  { prev :: [ a ]
  , next :: [ a ]
  } deriving ( Show )

--

cursor :: [ a ] -> Cursor a
cursor = Cursor []

extract :: Cursor a -> [ a ]
extract = reverse . prev

extractLength :: Cursor a -> Word
extractLength = fromIntegral . length . prev

--

goNext :: Cursor a -> Cursor a
goNext ( Cursor ps []     ) = Cursor ps       []
goNext ( Cursor ps (n:ns) ) = Cursor ( n:ps ) ns

goPrev :: Cursor a -> Cursor a
goPrev ( Cursor []     ns ) = Cursor [] ns
goPrev ( Cursor (p:ps) ns ) = Cursor ps ( p:ns )

--

peek :: Word -> Cursor a -> [ a ]
peek n ( Cursor _ []  ) = []
peek n ( Cursor _ ns )  = take ( fromIntegral n ) ns

peek2 :: Cursor a -> Maybe ( a, a )
peek2 c = if length el == 2 then Just ( el !! 0, el !! 1 ) else Nothing
  where el = peek 2 c

--

collapsable :: ( Char, Char ) -> Bool
collapsable ( c1, c2 ) = ( c1 /= c2 ) && ( toLower c1 == toLower c2 )

delete :: Word -> Cursor a -> Cursor a
delete n ( Cursor ps ns ) = Cursor ps $ drop ( fromIntegral n ) ns

collapse :: Cursor Char -> Cursor Char
collapse ( Cursor ps [] ) = Cursor ps []
collapse c
  | maybe False collapsable $ peek2 c = collapse $ goPrev $ delete 2 c
  | otherwise                         = collapse $ goNext c
