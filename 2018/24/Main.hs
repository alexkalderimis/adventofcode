{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

import           Elves
import           Elves.Advent

import Data.Ord
import Data.Tuple (swap)
import Data.Monoid
import Control.Applicative
import Text.Parser.Char (letter, space, newline, text)
import Text.Parser.Token (decimal, comma)
import Text.Parser.Combinators (choice, between, sepBy1, sepEndBy1)
import Data.Attoparsec.Text ((<?>), takeText)
import Data.Attoparsec.Combinator (lookAhead)
import Control.Lens
import qualified Data.List as L
import Control.Lens.Combinators (sumOf)
import Control.Monad.State.Strict
import Control.Lens.TH
import Data.Maybe
import           Data.Set     (Set)
import qualified Data.Set as S
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Text    (Text)
import qualified Data.Text as Text

import qualified Debug.Trace as Debug

data Side = ImmuneSystem | Infection deriving (Show, Eq, Ord)

data Group = Group
  { _groupSize       :: Int
  , _groupHP         :: Int
  , _groupAttack     :: (Int, Text)
  , _groupInitiative :: Int
  , _groupImmunities :: Set Text
  , _groupWeaknesses :: Set Text
  } deriving (Show, Eq, Ord)

makeLenses ''Group

effectivePower :: Group -> Int
effectivePower = (*) <$> view groupSize <*> view (groupAttack . _1)

type GroupId = Int
type Army = Map GroupId Group

data BattleState a = Input { _immuneSystem :: a , _infection :: a
                           } deriving (Show, Eq, Functor)

makeLenses ''BattleState

type Input = BattleState Army

data Attack = Attack { attacking :: Side , attackerId :: GroupId, defenderId :: GroupId
                     } deriving (Show, Eq)

main :: IO ()
main = day 24 inputP pt1 pt2 test

inputP :: Parser Input
inputP = Input <$>             (header "Immune System" *> armyP <?> "immune-army")
               <*> (armySep *> (header "Infection"     *> armyP <?> "infection-army"))
  where
    armySep = (some newline <|> e "NEWLINE") <?> "army-separator"
    header t = (txt t *> txt ":" *> newline) <?> (Text.unpack t <> "-header")
    armyP = M.fromList . zip [1 ..] <$> ((groupP <?> "group") `sepEndBy1` (newline <?> "group-separator"))
    txt t = text t <|> e t

    e exp = takeText >>= \got -> fail (Text.unpack $ Text.unwords ["Expected", exp, "but got:", Text.take 10 got])

battle :: State Input Side
battle = do
  fight
  dead <- gets (fmap M.null)
  case dead of
    (Input True True) -> error "impossible"
    (Input True False) -> pure Infection
    (Input False True) -> pure ImmuneSystem
    (Input False False) -> battle

fight :: State Input ()
fight = do
  selections <- selectTargets
  mapM_ performAttack selections

boost :: Int -> Input -> Input
boost b = immuneSystem %~ M.map (groupAttack._1 +~ b)

selectTargets :: State Input [Attack]
selectTargets = fmap (fmap snd . L.sortBy (comparing (Down . fst)) . catMaybes) $ do
  getAttackers >>= locally . mapM (\(side, (gid, g)) -> do
    mt <- selectTarget side g
    return $ fmap ((,) (g ^. groupInitiative) . Attack side gid) mt)

getAttackers :: State Input [(Side, (GroupId, Group))]
getAttackers = do
  let tag k = zip (repeat k) . M.toList
  imms <- uses immuneSystem (tag ImmuneSystem)
  infs <- uses infection (tag Infection)
  return $ L.sortBy (comparing (attackOrder.snd.snd))
         $ imms <> infs
 where
   attackOrder = (,) <$> (Down . effectivePower) <*> (Down . view groupInitiative)

locally :: State s a -> State s a
locally ma = do
  s <- get
  r <- ma
  put s
  return r

selectTarget :: Side -> Group -> State Input (Maybe GroupId)
selectTarget side grp = do
  enemies <- uses (opponents side) (M.filter ((> 0) . damageCausedBy grp))
  if M.null enemies
     then pure Nothing
     else do let bestTarget = (,,) <$> damageCausedBy grp
                                   <*> effectivePower
                                   <*> view groupInitiative
             let e = fst $ L.maximumBy (comparing (bestTarget.snd)) (M.toList enemies)
             opponents side #%= M.delete e
             pure (Just e)

opponents ImmuneSystem = infection
opponents Infection = immuneSystem

allies ImmuneSystem = immuneSystem
allies Infection = infection

damageCausedBy :: Group -> Group -> Int
damageCausedBy attacker defender
  | S.member element (defender ^. groupImmunities) = 0
  | S.member element (defender ^. groupWeaknesses) = 2 * ep
  | otherwise = ep
  where
    ep = effectivePower attacker
    element = attacker ^. groupAttack._2

performAttack :: Attack -> State Input ()
performAttack Attack{..} = do
  mattacker <- uses (allies attacking) (M.lookup attackerId)
  mdefender <- uses (opponents attacking) (M.lookup defenderId)
  case (mattacker, mdefender) of
    (Just attacker, Just defender) -> do
      let losses = damageCausedBy attacker defender `div` defender ^. groupHP
          defender' = defender & groupSize -~ losses
      opponents attacking #%= if (defender' ^. groupSize > 0)
                              then M.insert defenderId defender'
                              else M.delete defenderId
    _ -> pure ()

groupP :: Parser Group
groupP = do
  n <- int <* text " units"
  hp <- text " each with " *> int <* text " hit points"
  (imm,weak) <- characteristics

  attack <- text " with an attack that does " *> attackP <* text " damage"
  init <- text " at initiative " *> int
  return $ Group n hp attack init imm weak 
 where
    bracketed = between (text " (") (text ")")

    damageKind = Text.pack <$> some letter
    attackP = (,) <$> int <*> (space *> damageKind)
    int = fromInteger <$> decimal
    characteristic what = text what
                          *> text " to "
                          *> (S.fromList <$> (damageKind `sepBy1` text ", "))
    characteristics = defaulting (mempty, mempty)
                      . bracketed
                      $ choice [ pair (characteristic "immune") (characteristic "weak")
                               , swap <$> pair (characteristic "weak") (characteristic "immune")
                               , (,) mempty <$> characteristic "weak"
                               , (,) <$> characteristic "immune" <*> pure mempty
                               ]
                               
    defaulting x = fmap (fromMaybe x) . optional
    pair a b = (,) <$> (a <* text "; ") <*> b

pt1 inp = do
  let (side,s) = runState battle inp
  putStrLn $ "Victor: " ++ show side
  putStrLn $ "with " ++ show (units s) ++ " units"

pt2 _ = print "not implemented"

units = let u fld = sumOf (folded.groupSize) . view fld
         in (+) <$> u immuneSystem <*> u infection

test = do
  let input imms infs = Input (M.fromList (zip [1 ..] imms)) (M.fromList (zip [1 ..] infs))
      group = Group { _groupSize = 17
                    , _groupHP = 5
                    , _groupWeaknesses = mempty
                    , _groupImmunities = mempty
                    , _groupInitiative = 3
                    , _groupAttack = (13, "")
                    }
      
  describe "effectivePower" $ do
    it "is the product of unit strength and attack damage" $ do
      let g = group & groupSize .~ 18
                    & groupAttack._1 .~ 8
      effectivePower g `shouldBe` 144
  describe "performAttack" $ do
    it "eliminates the correct number of units" $ do
      let attacker = group & groupSize .~ 3
                           & groupAttack .~ (25, "radiation")
      let defender = group & groupSize .~ 10
                           & groupHP .~ 10
      let ret = execState (performAttack (Attack ImmuneSystem 1 1))
                          (input [attacker] [defender])
      ret ^. infection `shouldBe` M.singleton 1 (defender & groupSize .~ 3)
    it "does nothing if the attacker was killed first" $ do
      let attacker = group & groupSize .~ 3
                           & groupAttack .~ (25, "radiation")
          s = input [] [group]
          ret = execState (performAttack (Attack ImmuneSystem 1 1)) s
      ret `shouldBe` s
      
  describe "selectTargets" $ do
    it "selects the correct groups to attack" $ do
      let (Right inp) = parseOnly inputP exampleInput
          pairings = evalState selectTargets inp
      pairings `shouldBe` [Attack Infection 2 2
                          ,Attack ImmuneSystem 2 1
                          ,Attack ImmuneSystem 1 2
                          ,Attack Infection 1 1
                          ]
  describe "selectTarget" $ do
    it "selects the correct group for group-D to attack" $ do
      let (Right inp) = parseOnly inputP exampleInput
          (Just attacker) = inp ^. infection . at 2
          mdefender = evalState (selectTarget Infection attacker) inp
      mdefender `shouldBe` Just 2
    it "selects the correct group for group-B to attack" $ do
      let (Right inp) = parseOnly inputP exampleInput
          (Just attacker) = inp ^. immuneSystem . at 2
          mdefender = evalState (selectTarget ImmuneSystem attacker) inp
      mdefender `shouldBe` Just 1
    it "no group is selected if no enemies are available" $ do
      let attacker = group
          s = input [attacker] []
          mdefender = evalState (selectTarget ImmuneSystem attacker) s
      mdefender `shouldBe` Nothing
    it "no group is selected if no damage would be caused" $ do
      let attacker = group & groupAttack .~ (25, "radiation")
          defender = group & groupImmunities %~ S.insert "radiation"
          mdefender = evalState (selectTarget ImmuneSystem attacker) (input [attacker] [defender])
      mdefender `shouldBe` Nothing

  describe "boost" $ do
    it "runs the boosted battle correctly" $ do
      let (Right inp) = parseOnly inputP exampleInput
          (victor,s) = runState battle (boost 1570 inp)
      (victor, units s) `shouldBe` (ImmuneSystem, 51)
  describe "battle" $ do
    it "predicts victory for the infection in the example battle" $ do
      let (Right inp) = parseOnly inputP exampleInput
          victor = evalState battle inp
      victor `shouldBe` Infection
    it "gets the magnitude of the victory correct" $ do
      let (Right inp) = parseOnly inputP exampleInput
          s = execState battle inp
      units s `shouldBe` 5216
  describe "fight" $ do
    let stages = takeWhile (allOf (immuneSystem <> infection) (> 0) . fmap M.size)
               . iterate (execState fight)
    it "produces the right sequence of outcomes" $ do
      let (Right inp) = parseOnly inputP exampleInput
          sizeSummary = fmap $ map (view groupSize) . M.elems
          res = sizeSummary <$> stages inp
      res `shouldBe` [Input [17,989] [801,4485]
                     ,Input [905] [797,4434]
                     ,Input [761] [793,4434]
                     ,Input [618] [789,4434]
                     ,Input [475] [786,4434]
                     ,Input [333] [784,4434]
                     ,Input [191] [783,4434]
                     ,Input [49]  [782,4434]
                     ]
    it "runs for the correct number of stages" $ do
      let (Right inp) = parseOnly inputP exampleInput
          res = stages inp
      length res `shouldBe` 8
  describe "getAttackers" $ do
    it "selects the groups to attack in the correct order" $ do
      let (Right inp) = parseOnly inputP exampleInput
          attackers = fmap (fmap fst) $ evalState getAttackers inp
      attackers `shouldBe` [(Infection, 1)
                           ,(ImmuneSystem, 1)
                           ,(Infection, 2)
                           ,(ImmuneSystem, 2)
                           ]
  describe "groupP" $ do
    describe "example group" $ do
      let t = Text.unwords ["18 units each with 729 hit points"
                        ,"(weak to fire; immune to cold, slashing)"
                        ,"with an attack that does 8 radiation damage at initiative 10"
                        ]
      let g = Group { _groupSize = 18
                    , _groupHP = 729
                    , _groupWeaknesses = S.singleton "fire"
                    , _groupImmunities = S.fromList ["cold", "slashing"]
                    , _groupInitiative = 10
                    , _groupAttack = (8, "radiation")
                    }
      it "parses correctly" $ do
        parseOnly groupP t `shouldBe` Right g
    describe "group without characteristics" $ do
      let t = "949 units each with 3117 hit points with an attack that does 29 fire damage at initiative 10"
      let g = Group { _groupSize = 949
                    , _groupHP = 3117
                    , _groupWeaknesses = mempty
                    , _groupImmunities = mempty
                    , _groupInitiative = 10
                    , _groupAttack = (29, "fire")
                    }
      it "parses correctly" $ do
        parseOnly groupP t `shouldBe` Right g
    describe "groupA" $ do
      let expected = Group 17 5390 (4507, "fire") 2 mempty
                           (S.fromList ["radiation", "bludgeoning"])
      it "should be as expected" $ do
        parseOnly groupP groupA
        `shouldBe` Right expected
    describe "groupB" $ do
      let expected = Group 989 1274 (25, "slashing") 3
                     (S.singleton "fire")
                     (S.fromList ["bludgeoning", "slashing"])
      it "should be as expected" $ do
        parseOnly groupP groupB
        `shouldBe` Right expected

  describe "inputP" $ do
    let ei = parseOnly inputP exampleInput
    let groups = let n fld = M.size . view fld
                  in (+) <$> n immuneSystem <*> n infection
    it "should have 4 groups" $ do
      fmap groups ei `shouldBe` Right 4
    it "should have 17 + 989 + 801 + 4485 units" $ do
      fmap units ei `shouldBe` Right (17 + 989 + 801 + 4485)

exampleInput = Text.unlines
  ["Immune System:", groupA, groupB
  ,""
  ,"Infection:", groupC, groupD
  ]

groupA, groupB, groupC, groupD :: Text
groupA = 
   "17 units each with 5390 hit points (weak to radiation, bludgeoning) with"
   <> " an attack that does 4507 fire damage at initiative 2"
groupB = 
   "989 units each with 1274 hit points (immune to fire; weak to bludgeoning,"
   <> " slashing) with an attack that does 25 slashing damage at initiative 3"
groupC = 
   "801 units each with 4706 hit points (weak to radiation) with an attack"
   <> " that does 116 bludgeoning damage at initiative 1"
groupD =
   "4485 units each with 2961 hit points (immune to radiation; weak to fire,"
   <> " cold) with an attack that does 12 slashing damage at initiative 4"
