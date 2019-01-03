{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

import           Elves
import           Elves.Advent

import Control.Monad.Trans.Maybe
import           Control.Applicative
import           Control.Lens hiding (element, elements)
import           Control.Lens.Combinators   (sumOf)
import           Control.Lens.TH
import           Control.Monad.State.Strict
import           Data.Attoparsec.Text       (decimal, takeText, takeWhile1,
                                             (<?>))
import           Data.Char                  (isLetter)
import qualified Data.List                  as L
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as M
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.Set                   (Set)
import qualified Data.Set                   as S
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Data.Tuple                 (swap)
import           Text.Parser.Char           (newline, space, text)
import           Text.Parser.Combinators    (between, choice, sepBy1, sepEndBy1)
import           Text.Parser.Token          (comma)
import Test.QuickCheck

-- import qualified Debug.Trace                as Debug

data Side = Stalemate Int | Draw | ImmuneSystem | Infection deriving (Show, Eq, Ord)

data Group = Group
  { _groupSize       :: Int
  , _groupHP         :: Int
  , _groupAttack     :: (Int, Text)
  , _groupInitiative :: Int
  , _groupImmunities :: Set Text
  , _groupWeaknesses :: Set Text
  } deriving (Show, Eq, Ord)

instance Arbitrary Group where
  arbitrary = Group <$> int 10000
                    <*> int 100000
                    <*> (int 500 <#> arbitraryElement)
                    <*> int 1000
                    <*> arbitraryElementSet
                    <*> arbitraryElementSet
                where
                  int mv = scale (min mv) arbitrarySizedNatural
                  arbitraryElementSet = int 10 >>= \n -> (S.fromList <$> vectorOf n arbitraryElement)
                  arbitraryElement = elements ["frost", "slashing", "acid", "fire", "ice", "bludgeoning", "slashing"
                                              ,"poison", "cold", "radiation", "heat", "psychic", "corrosive"
                                              ,"shock", "necrotic", "piercing", "lightning", "radiant"
                                              ]

makeLenses ''Group

power :: Lens' (Int, Text) Int
power = _1
element :: Lens' (Int, Text) Text
element = _2

effectivePower :: Group -> Int
effectivePower = product . toListOf (groupSize <> groupAttack.power)

type GroupId = Int
type Army = Map GroupId Group

data BattleState a = Input { _immuneSystem :: a , _infection :: a
                           } deriving (Show, Eq, Functor)

makeLenses ''BattleState

type Input = BattleState Army

-- we use indirection to specify the attack input in order
-- to maintain consistency across the fight. If the attacker
-- is damaged or killed before the attack, then that will
-- change the outcome.
data Attack = Attack
  { attacking :: Side
  , attackerId :: GroupId
  , defenderId :: GroupId
  } deriving (Show, Eq)

main :: IO ()
main = day 24 inputP pt1 pt2 test
  where
    pt1 inp = do
      let (side,s) = runState battle inp
      putStrLn $ "Victor: " ++ show side
      putStrLn $ "with " ++ show (units s) ++ " units"

    pt2 inp = do
      let bst = minimalBoost inp
      putStrLn $ "Minimal boost: " ++ show bst
      pt1 (boost bst inp)

inputP :: Parser Input
inputP = Input <$>             (header "Immune System" *> armyP <?> "immune-army")
               <*> (armySep *> (header "Infection"     *> armyP <?> "infection-army"))
  where
    armySep = (some newline <|> e "NEWLINE") <?> "army-separator"
    header t = (txt t *> txt ":" *> newline) <?> (Text.unpack t <> "-header")
    armyP = army <$> (groupP `sepEndBy1` newline)
    txt t = text t <|> e t

    e exp = takeText >>= \got -> fail (Text.unpack $ Text.unwords ["Expected", exp, "but got:", Text.take 10 got])

battle :: State Input Side
battle = do
  s <- get
  fight
  s' <- get
  dead <- gets (fmap M.null)
  case dead of
      (Input True True)   -> pure Draw
      (Input True False)  -> pure Infection
      (Input False True)  -> pure ImmuneSystem
      (Input False False) -> if s == s' then pure (Stalemate (units s)) else battle

fight :: State Input ()
fight = selectTargets >>= mapM_ performAttack

boost :: Int -> Input -> Input
boost b = immuneSystem %~ M.map (groupAttack.power +~ b)

-- use binary search to find the minimum boost
minimalBoost :: Input -> Int
minimalBoost inp = minimalBinarySearch ((== ImmuneSystem) . victorWithBoost) (0, maxBoost)
  where
    maxBoost = maximum $ fmap ((*) <$> view groupHP <*> view groupSize)
                       $ M.elems
                       $ inp ^. infection
    victorWithBoost b = evalState battle (boost b inp)

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
  enemies <- uses (opponents side) (M.filter canBeDamaged)
  if M.null enemies
     then pure Nothing
     else do let e = bestTarget enemies
             Just e <$ (opponents side #%= M.delete e)
  where
     canBeDamaged = (> 0) . damageCausedBy grp
     targetOrder  = (,,) <$> damageCausedBy grp <*> effectivePower <*> view groupInitiative
     -- we break all ordering ties with the index to provide a stable total order
     bestTarget   = fst . L.maximumBy (comparing (\(i,g) -> (targetOrder g, Down i))) . M.toList

opponents ImmuneSystem = infection
opponents Infection    = immuneSystem

allies ImmuneSystem = immuneSystem
allies Infection    = infection

damageCausedBy :: Group -> Group -> Int
damageCausedBy attacker defender
  | defender^.groupImmunities.contains el = 0
  | defender^.groupWeaknesses.contains el = 2 * ep
  | otherwise                             = ep
  where
    ep = effectivePower attacker
    el = attacker ^. groupAttack.element

performAttack :: Attack -> State Input ()
performAttack Attack{..} = void $ runMaybeT $ do
  a <- MaybeT $ uses (allies attacking) (M.lookup attackerId)
  d <- MaybeT $ uses (opponents attacking) (M.lookup defenderId)
  let losses = damageCausedBy a d `div` d ^. groupHP
      action = if | losses == 0              -> id
                  | losses >= d ^. groupSize -> sans defenderId
                  | otherwise -> ix defenderId %~ (groupSize -~ losses) 
  lift (opponents attacking #%= action)

groupP :: Parser Group
groupP = do
  _groupSize       <- decimal <* " units"
  _groupHP         <- " each with " *> decimal <* " hit points"
  (_groupImmunities,_groupWeaknesses) <- characteristics
  _groupAttack     <- " with an attack that does " *> attackP <* " damage"
  _groupInitiative <- " at initiative " *> decimal
  return $ Group{..}
 where
    damageKind = takeWhile1 isLetter
    attackP    = (,) <$> decimal  <*> (space *> damageKind)
    -- most complex bit is handling the fact that characteristics can come in either order, and
    -- either side (or both) can be omitted)
    characteristics =
      defaulting (mempty, mempty)
      . bracketed
      $ choice [ pair (characteristic "immune") (characteristic "weak")
               , swap <$> pair (characteristic "weak") (characteristic "immune")
               , (,) mempty <$> characteristic "weak"
               , (,) <$> characteristic "immune" <*> pure mempty
               ]
    characteristic what = text what
                          *> text " to "
                          *> (S.fromList <$> (damageKind `sepBy1` text ", "))

    defaulting x = fmap (fromMaybe x) . optional
    pair a b     = (,) <$> (a <* text "; ") <*> b
    bracketed    = between (text " (") (text ")")

army :: [Group] -> Army
army = M.fromList . zip [1 ..]

units :: Input -> Int
units = sumOf (traverse.folded.groupSize) . toListOf (immuneSystem <> infection)

test = do
  let -- construct an Input from two lists of groups
      input imms infs = Input (army imms) (army infs)
      -- a base group to derive others from. Every number is prime to help spot bugs
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
                    & groupAttack.power .~ 8
      effectivePower g `shouldBe` 144
    it "is 75 for the example given" $ do
      let attacker = group & groupSize .~ 3
                           & groupAttack .~ (25, "radiation")
      effectivePower attacker `shouldBe` 75

  describe "performAttack" $ do
    it "eliminates the correct number of units" $ do
      let attacker = group & groupSize .~ 3
                           & groupAttack .~ (25, "radiation")
      let defender = group & groupSize .~ 10
                           & groupHP .~ 10
                           & groupImmunities.contains "radiation" .~ False
                           & groupWeaknesses.contains "radiation" .~ False
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
    it "selects at most one attacker for each defender" $ do
      let inp = input [group, group] [group]
          pairings = evalState selectTargets inp
      pairings `shouldBe` [Attack ImmuneSystem 1 1
                          ,Attack Infection 1 1
                          ]
    it "selects the correct groups to attack" $ do
      let (Right inp) = parseOnly inputP exampleInput
          pairings = evalState selectTargets inp
      pairings `shouldBe` [Attack Infection 2 2
                          ,Attack ImmuneSystem 2 1
                          ,Attack ImmuneSystem 1 2
                          ,Attack Infection 1 1
                          ]

    it "ensures that each group has selected zero or one groups to attack, and each group is being attacked by zero or one groups." $
       property $ \goodies baddies ->
         let s = input goodies baddies
             maxGoodId = length goodies
             maxBadId = length baddies
             attacks = evalState selectTargets s
             inRange x (lb,ub) = lb <= x && x <= ub
          in    and [attackerId `inRange` (0,maxGoodId) && defenderId `inRange` (0,maxBadId) | Attack{attacking = ImmuneSystem, ..} <- attacks]
             && and [attackerId `inRange` (0,maxBadId) && defenderId `inRange` (0,maxGoodId) | Attack{attacking = Infection, ..} <- attacks]
             && and [ n == 1 | n <- M.elems $ M.fromListWith (+) [((attacking, attackerId), 1) | Attack{..} <- attacks] ]
             && and [ n == 1 | n <- M.elems $ M.fromListWith (+) [((attacking, defenderId), 1) | Attack{..} <- attacks] ]

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
    it "prefers targets that are weak to the attack element" $ do
      let attacker = group & groupAttack.element .~ "frost"
          d1 = group
          d2 = group & groupWeaknesses.contains "frost" .~ True
          d3 = group & groupImmunities.contains "frost" .~ True
          mdefender = evalState (selectTarget ImmuneSystem attacker) (input [attacker] [d1,d2,d3])
      mdefender `shouldBe` Just 2
    it "prefers targets that are not immune to the attack element" $ do
      let attacker = group & groupAttack.element .~ "frost"
          d1 = group
          d2 = group & groupImmunities.contains "frost" .~ True
          mdefender = evalState (selectTarget ImmuneSystem attacker) (input [attacker] [d1,d2])
      mdefender `shouldBe` Just 1
    it "prefers targets that are stronger" $ do
      let attacker = group & groupAttack.element .~ "frost"
          d1 = group & groupAttack.power .~ 100
          d2 = group & groupAttack.power .~ 200
          d3 = group & groupAttack.power .~ 150
          mdefender = evalState (selectTarget ImmuneSystem attacker) (input [attacker] [d1,d2,d3])
      mdefender `shouldBe` Just 2
    it "prefers targets that have higher initiative" $ do
      let attacker = group & groupAttack.element .~ "frost"
          d1 = group & groupInitiative .~ 100
          d2 = group & groupInitiative .~ 200
          d3 = group & groupInitiative .~ 300
          mdefender = evalState (selectTarget ImmuneSystem attacker) (input [attacker] [d1,d2,d3])
      mdefender `shouldBe` Just 3
    it "selects the susceptible, strong high-initiative target" $ do
      let attacker = group & groupAttack.element .~ "frost"

          immune      = groupImmunities.contains "frost" .~ True
          susceptible = groupWeaknesses.contains "frost" .~ True
          neither     = id

          strong = groupAttack.power .~ 300
          medium = groupAttack.power .~ 200
          weak   = groupAttack.power .~ 100

          highInit = groupInitiative .~ 30
          medInit  = groupInitiative .~ 20
          lowInit  = groupInitiative .~ 10

          ds = [group & a.b.c | a <- [immune,   susceptible, neither ]
                              , b <- [strong,   medium,      weak    ]
                              , c <- [lowInit,  medInit,     highInit]
               ]
          mdefender = evalState (selectTarget ImmuneSystem attacker) (input [attacker] ds)
          di = L.elemIndex (susceptible . strong . highInit $ group) ds
      mdefender `shouldBe` (succ <$> di)

    it "does not select any group if no enemies are available" $ do
      let attacker = group
          s = input [attacker] []
          mdefender = evalState (selectTarget ImmuneSystem attacker) s
      mdefender `shouldBe` Nothing
    it "does not select any group if no damage would be caused" $ do
      let attacker = group & groupAttack.element .~ "radiation"
          defender = group & groupImmunities.contains "radiation" .~ True
          mdefender = evalState (selectTarget ImmuneSystem attacker) (input [attacker] [defender])
      mdefender `shouldBe` Nothing

  describe "minimalBoost" $ do
    let (Right inp) = parseOnly inputP exampleInput
        mb = minimalBoost inp
    it "is at most the example value given" $ do
      mb <= 1570 `shouldBe` True
    it "can find a minimal boost that makes sure the immune system wins" $ do
      evalState battle (boost mb inp) `shouldBe` ImmuneSystem
    it "is the minimum boost needed" $ do
      evalState battle (boost (mb - 1) inp) `shouldBe` Infection

  describe "boost" $ do
    let (Right inp) = parseOnly inputP exampleInput
    let stages = takeWhile (allOf (immuneSystem <> infection) (> 0) . fmap M.size)
               . iterate (execState fight)
        sizeSummary = fmap $ map (view groupSize) . M.elems
    it "runs the boosted battle correctly" $ do
      let (victor,s) = runState battle (boost 1570 inp)
      (victor, units s) `shouldBe` (ImmuneSystem, 51)
    it "runs the boosted fights correctly" $ do
      -- we only have a summary, so we just test what we know
      let res = sizeSummary <$> stages (boost 1570 inp)
      res `shouldStartWith` [Input [17,989] [801,4485]
                            ,Input [8,905] [466,4453]
                            ,Input [876] [160,4453]
                            ]
      res `shouldContain`   [Input [64] [19,214]
                            ,Input [60] [19,182]
                            ,Input [60] [182]
                            ,Input [57] [152]
                            ]
      res `shouldEndWith`   [Input [51] [40]
                            ,Input [51] [13]
                            ]

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
