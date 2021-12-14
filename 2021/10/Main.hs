{-# LANGUAGE OverloadedStrings #-}

import qualified Data.List as L
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Attoparsec.Text as A
import           Control.Applicative.Combinators (sepBy1)
import           Text.Parser.Char (char, newline)

import Elves
import Elves.Advent

data Style
  = Brace       -- {}
  | Bracket     -- []
  | Angle       -- <>
  | Parenthesis -- ()
  deriving (Eq, Show)

data Action = Open | Close deriving (Show, Eq)
data Token = Token { style :: !Style, action :: !Action } deriving (Show, Eq)

newtype Line = L { tokens :: [Token] } deriving (Eq, Show)

data LineState = OK
               | Incomplete Line
               | Corrupt Style Int Token
               deriving (Show, Eq)

main :: IO ()
main = day 10 parser pt1 pt2 test
  where
    parser = lineP `sepBy1` newline
    pt1 lines = let tokens = [token | Corrupt _ _ token <- fmap validate lines]
                 in print . sum . fmap pointsValue $ tokens
    pt2 lines = let scores = [autocompleteScore line | Incomplete line <- fmap validate lines]
                 in putStrLn . maybe "None" show $ median scores

test = do
  let Right lines = mapM (parseOnly lineP) exampleInput
  it "validates the corrupt lines correctly" $ do
    let messages = [showError line state | (line, state) <- zip lines (validate <$> lines), Corrupt{} <- [state]]

    messages `shouldMatchList` [ "{([(<{}[<>[]}>{[]{[(<()> - Expected ], but found } instead."
                               , "[[<[([]))<([[{}[[()]]] - Expected ], but found ) instead."
                               , "[{[{({}]{}}([{[{{{}}([] - Expected ), but found ] instead."
                               , "[<(<(<(<{}))><([]([]() - Expected >, but found ) instead."
                               , "<{([([[(<>()){}]>(<<{{ - Expected ], but found > instead."
                               ]
  it "calculates the correct points total for the example input" $ do
    let tokens = [token | Corrupt _ _ token <- fmap validate lines]
    sum (pointsValue <$> tokens) `shouldBe` 26397

  it "can repair incomplete lines" $ do
    let messages = [showError line state | (line, state) <- zip lines (validate <$> lines), Incomplete{} <- [state]]
    messages `shouldMatchList` [ "[({(<(())[]>[[{[]{<()<>> - Complete by adding }}]])})]."
                               , "[(()[<>])]({[<{<<[]>>( - Complete by adding )}>]})."
                               , "(((({<>}<{<{<>}{[]{[]{} - Complete by adding }}>}>))))."
                               , "{<[[]]>}<{[{[{[]{()[[[] - Complete by adding ]]}}]}]}>."
                               , "<{([{{}}[<[[[<>{}]]]>[]] - Complete by adding ])}>."
                               ]
  it "can determine the median autocomplete score" $ do
    let scores = [autocompleteScore completion | Incomplete completion <- fmap validate lines]
    scores `shouldMatchList` [288957, 5566, 1480781, 995444, 294]
    median scores `shouldBe` Just 288957

exampleInput :: [Text]
exampleInput = [ "[({(<(())[]>[[{[]{<()<>>"
               , "[(()[<>])]({[<{<<[]>>("
               , "{([(<{}[<>[]}>{[]{[(<()>"
               , "(((({<>}<{<{<>}{[]{[]{}"
               , "[[<[([]))<([[{}[[()]]]"
               , "[{[{({}]{}}([{[{{{}}([]"
               , "{<[[]]>}<{[{[{[]{()[[[]"
               , "[<(<(<(<{}))><([]([]()"
               , "<{([([[(<>()){}]>(<<{{"
               , "<{([{{}}[<[[[<>{}]]]>[]]"
               ]

showError :: Line -> LineState -> Text
showError line OK = "OK"
showError line (Incomplete completion) = mconcat [ showLine line
                                                 , " - Complete by adding "
                                                 , showLine completion
                                                 , "."
                                                 ]
showError line (Corrupt st at got) = let action' = if style got == st then Open else Close
                                      in mconcat [ showLine line
                                                 , " - Expected "
                                                 , showToken (Token st action')
                                                 , ", but found "
                                                 , showToken got
                                                 , " instead."
                                                 ]

showToken :: Token -> Text
showToken (Token Brace Open) = "{"
showToken (Token Brace Close) = "}"
showToken (Token Bracket Open) = "["
showToken (Token Bracket Close) = "]"
showToken (Token Angle Open) = "<"
showToken (Token Angle Close) = ">"
showToken (Token Parenthesis Open) = "("
showToken (Token Parenthesis Close) = ")"

pointsValue :: Token -> Int
pointsValue t = case style t of
  Parenthesis -> 3
  Bracket -> 57
  Brace -> 1197
  Angle -> 25137

autocompleteScore :: Line -> Int
autocompleteScore = L.foldl' go 0 . tokens
  where
    value Parenthesis = 1
    value Bracket = 2
    value Brace = 3
    value Angle = 4
    go n t = n * 5 + value (style t)

showLine :: Line -> Text
showLine = mconcat . fmap showToken . tokens

lineP :: Parser Line
lineP = let characters = [(Brace, "{}"), (Bracket, "[]"), (Angle, "<>"), (Parenthesis, "()")]
            choices = [ Token s a <$ char c | (s, chrs) <- characters, (a, c) <- zip [Open, Close] chrs]
         in L <$> some (A.choice choices)

validate :: Line -> LineState
validate = go OK [] . zip [0..] . tokens
  where
    go OK [] []        = OK
    go OK stack []     = Incomplete $ L [Token s Close | s <- stack]
    go OK stack ((n, t):ts) =
       case t of Token s Open -> go OK (s : stack) ts
                 Token s Close -> case stack of
                   [] -> Corrupt s n t
                   (s' : stack') -> if s' == s then go OK stack' ts
                                               else Corrupt s' n t
