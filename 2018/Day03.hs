{-# LANGUAGE RecordWildCards #-}
--
import           Data.Bool ( bool )
import           Data.Bifunctor ( first, second )
import           Data.List.Split ( splitOn )

import           Data.Map as Map ( Map, alter )
import qualified Data.Map as Map ( filter )

import           Data.Set as Set
                 ( Set, insert, singleton, size, union, toList, (\\) )
--

main :: IO ()
main = do
  input <- lines <$> readFile "Day03.input"

  let claims = enterClaims mempty $ fmap parseClaim input

  putStr "Part1: "
  print $ overlapping claims

  putStr "Part2: "
  print $ nonoverlapping claims

--

data Claim = C
  { cid :: Word
  , cx  :: Word
  , cy  :: Word
  , cx' :: Word
  , cy' :: Word
  } deriving ( Show )

type ClothClaims
  = Map ( Word, Word ) ( Set Word )

--

overlapping :: ClothClaims -> Word
overlapping = foldr ( \a b -> if size a > 1 then b + 1 else b ) 0

nonoverlapping :: ClothClaims -> Word
nonoverlapping cc = head $ toList $ free \\ over
  where
    ( free, over ) = foldr
      ( \x -> bool second first ( size x == 1 ) ( union x ) ) mempty cc

--

parseClaim :: String -> Claim
parseClaim = p . filter (/="") . splitOn " " . fmap r
  where
    r c  = if c `elem` "#@,:x" then ' ' else c
    p cs = C {..}
      where
        rcs  = map read cs
        cid  = rcs !! 0
        cx   = rcs !! 1 + 1
        cy   = rcs !! 2 + 1
        cx'  = rcs !! 3 + cx - 1
        cy'  = rcs !! 4 + cy - 1

--

enterClaim :: Claim -> ClothClaims -> ClothClaims
enterClaim C{..} =
  flip ( foldr ( alter $ Just . maybe ( singleton cid ) ( insert cid ) ) )
  [ (x, y) | x <- [cx..cx'], y <- [cy..cy'] ]

enterClaims :: ClothClaims -> [ Claim ] -> ClothClaims
enterClaims = foldr enterClaim
