import qualified Text.ParserCombinators.ReadP as R
import Control.Applicative
import Text.Read (readsPrec)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Monoid
import Data.Maybe

data Claim = Claim { claimId :: ClaimId
                   , claimOffset :: Offset
                   , claimSize :: Size
                   } deriving (Show)

newtype ClaimId = ClaimId { unclaimId :: Int } deriving (Show, Eq, Ord)

data Offset = Offset { offsetX :: Int, offsetY :: Int } deriving Show
data Size = Size { sizeWidth :: Int, sizeHeight :: Int } deriving Show

type SquareInch = (Int,Int)

parseClaim :: String -> Maybe Claim
parseClaim = fmap fst . listToMaybe . R.readP_to_S claimP

-- Parse claims, of the form:
-- #1 @ 1,3: 4x4
-- #2 @ 3,1: 4x4
-- #3 @ 5,5: 2x2
claimP :: R.ReadP Claim
claimP = Claim <$> claimIdP
               <*> (R.string " @ " >> offsetP)
               <*> (R.string ": " >> sizeP)

claimIdP :: R.ReadP ClaimId
claimIdP = R.char '#' >> (ClaimId <$> int)

offsetP :: R.ReadP Offset
offsetP = Offset <$> int <*> (R.char ',' >> int)

sizeP :: R.ReadP Size
sizeP = Size <$> int <*> (R.char 'x' >> int)

int :: R.ReadP Int
int = R.readS_to_P (readsPrec 10)

squareInches :: Claim -> [SquareInch]
squareInches claim = [ (x + dx, y + dy) | x <- [0 .. w claim]
                                        , y <- [0 .. h claim]

                     ]
  where
    w = subtract 1 . sizeWidth . claimSize
    h = subtract 1 . sizeHeight . claimSize
    Offset dx dy = claimOffset claim

type OverlappedClaims = M.Map SquareInch (S.Set ClaimId)

overlaps :: OverlappedClaims -> S.Set SquareInch
overlaps = M.keysSet . M.filter ((> 1) . S.size)

uncontestedClaims :: OverlappedClaims -> S.Set ClaimId
uncontestedClaims = M.keysSet . M.filter (== 1) . foldl f mempty
  where f m = M.unionWith max m . neighbourhood
        neighbourhood set = M.fromSet (pure $ S.size set) set

overlapAllClaims :: [Claim] -> OverlappedClaims
overlapAllClaims = M.unionsWith (<>) . fmap coverage
  where coverage claim = M.fromSet (pure . S.singleton $ claimId claim)
                       . S.fromList
                       $ squareInches claim

example :: Maybe (S.Set SquareInch, S.Set ClaimId)
example = do
  claims <- traverse parseClaim ["#1 @ 1,3: 4x4"
                                ,"#2 @ 3,1: 4x4"
                                ,"#3 @ 5,5: 2x2"
                                ]
  let allClaims = overlapAllClaims claims
  return (overlaps allClaims, uncontestedClaims allClaims)

main :: IO ()
main = do
  inputs <- lines <$> getContents
  let claims = traverse parseClaim inputs
  case claims of
    Nothing -> error "Could not read claims"
    Just cs -> do let allClaims = overlapAllClaims cs
                  putStrLn $ "no. of overlaps: " <> (show . S.size $ overlaps allClaims)
                  putStrLn "Uncontested claims:"
                  putStrLn "------------------"
                  mapM_ print (uncontestedClaims allClaims)

