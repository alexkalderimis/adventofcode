{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative.Combinators
import           Data.Attoparsec.Text            (decimal)
import qualified Data.Attoparsec.Text            as A
import           Data.Char
import           Data.Foldable                   (maximumBy)
import           Data.HashMap.Strict             (HashMap)
import qualified Data.HashMap.Strict             as M
import qualified Data.HashSet                    as S
import qualified Data.List                       as L
import qualified Data.List.NonEmpty              as NE
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Data.Tree                       (Tree (..))
import           Text.Parser.Char                (newline)

import           Elves
import           Elves.Advent

type Name = Text
type Happiness = Sum Int
type Set = S.HashSet
type Map = HashMap

type Friendships = Map Name (Map Name Happiness)
                 --     ^         ^      ^
                 --     +-- The person who has the opinion, a
                 --               |      |
                 --               +-- The person the opinion is of, b
                 --                      |
                 --                      +-- The value of b to a
type Seating = ([(Name, Happiness)], Happiness)
             --   ^        ^            ^
             --   +--- Who we seated    |
             --            |            |
             --            +-- How much Happiness they added by being seated,
             --                considering who was seated before them
             --                         |
             --                         +-- The total Happiness of this arrangement
             -- usefully, this is a monoid, where adding a link updates the total.

-- this is a back-pack problem. There may be a more efficient strategy, but this
-- approach only takes 150ms, so it doesn't seem to be worth it.
main :: IO ()
main = day 12 parser pt1 pt2 test
  where
    pt1 = print . seatGuests
    pt2 = print . breakMinimalLink . seatGuests

test = do
  describe "pt-1 example" $ do
    let mfs = parseOnly parser exampleInput
    specify "the best arrangement has a value of 330" $ do
      (snd . seatGuests <$> mfs) `shouldBe` Right 330
  describe "pt-2 example" $ do
    let mfs = parseOnly parser exampleInput
    specify "the best arrangement with me has a value of 286" $ do
      (snd . breakMinimalLink . seatGuests <$> mfs) `shouldBe` Right 286

parser :: Parser Friendships
parser = fmap (M.fromListWith (<>)) (friendship `sepBy1` newline)
  where
    name = A.takeWhile isLetter
    friendship = do
      a <- name
      sign <- " would " *> choice [id <$ "gain", negate <$ "lose"] <* A.space
      happiness <- Sum . sign <$> decimal
      A.string " happiness units by sitting next to "
      b <- name <* "."
      return (a, (M.singleton b happiness))

guestNames :: Friendships -> Set Name
guestNames = S.fromList . M.keys

-- infinite search tree. Doesn't matter where we start, provided
-- that every guest has an opinion of every other.
searchTree :: Friendships -> Maybe (Tree (Name, Happiness))
searchTree m = fmap (node initCost) $ listToMaybe (M.keys m)
  where
    -- it costs nothing to seat the first guest. That cost comes when we close the loop
    initCost = Sum 0
    node h n = Node (n, h) [node (h' <> h'') n'
                             | (n',h') <- maybe [] M.toList (M.lookup n m)
                             , let h'' = fromMaybe mempty (M.lookup n' m >>= M.lookup n)
                             ]

-- seat all the guests, and close the circular table at the end. Choose the maximal arrangement
bestArrangement :: Tree (Name, Happiness) -> Set Name -> Seating
bestArrangement t = flip seat [t]
  where
    seat unseated = maybe mempty (maximumBy (comparing snd))
                  . NE.nonEmpty
                  . fmap (go unseated)
                  . filter (needed unseated)
    go s (Node (who, h) ts) = ([(who, h)], h) <> seat (S.delete who s) ts
    needed s t' = S.member (fst $ rootLabel t') s || (S.null s && name t == name t')
    name = fst . rootLabel

-- the first link is always 0, so that is ignored.
breakMinimalLink :: Seating -> Seating
breakMinimalLink (links, total)
  | length links < 2 = (newLink : links, total)
  | otherwise        = let (who, v)    = L.minimumBy (comparing snd) (tail links)
                           (pref,suff) = L.break ((== who) . fst) links
                        in ((pref <> [newLink] <> suff), total - v)
  where
    newLink = ("*", Sum 0)

seatGuests :: Friendships -> Seating
seatGuests fs = fromMaybe mempty
              $ liftA2 bestArrangement (searchTree fs) (pure $ guestNames fs)

exampleInput = T.unlines
  ["Alice would gain 54 happiness units by sitting next to Bob."
  ,"Alice would lose 79 happiness units by sitting next to Carol."
  ,"Alice would lose 2 happiness units by sitting next to David."
  ,"Bob would gain 83 happiness units by sitting next to Alice."
  ,"Bob would lose 7 happiness units by sitting next to Carol."
  ,"Bob would lose 63 happiness units by sitting next to David."
  ,"Carol would lose 62 happiness units by sitting next to Alice."
  ,"Carol would gain 60 happiness units by sitting next to Bob."
  ,"Carol would gain 55 happiness units by sitting next to David."
  ,"David would gain 46 happiness units by sitting next to Alice."
  ,"David would lose 7 happiness units by sitting next to Bob."
  ,"David would gain 41 happiness units by sitting next to Carol."
  ]
