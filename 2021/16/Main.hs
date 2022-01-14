{-# LANGUAGE OverloadedStrings #-}

import Data.Word
import qualified Data.List.Extra as L
import qualified Data.Foldable as F
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Attoparsec.Text as A
import           Text.Parser.Combinators (count)

import Elves
import Elves.Advent
import qualified Elves.SeqParser as SP

data Bit = I | O deriving (Show, Eq, Ord)
type Tokens = Seq Bit

newtype Version = Version { getVersion :: Word8 } deriving (Show, Eq, Ord)
newtype PackageID = PackageID Word8 deriving (Show, Eq, Ord)

data Packet = Packet { packetVersion :: !Version, packetPayload :: !Payload }
            deriving (Show, Eq)

data Operation = SumOp | ProductOp | Minimum | Maximum | GreaterThan | LessThan | Equal
            deriving (Show, Eq)

data Payload
  = Literal !Word
  | Operator !Operation [Packet]
  deriving (Show, Eq)

main :: IO ()
main = day 16 parser pt1 pt2 test
  where
    parser = A.takeText >>= parse
    pt1 = print . versionSum
    pt2 = print . evaluate

bits :: Char -> [Bit]
bits '0' = [ O, O, O, O ]
bits '1' = [ O, O, O, I ]
bits '2' = [ O, O, I, O ]
bits '3' = [ O, O, I, I ]
bits '4' = [ O, I, O, O ]
bits '5' = [ O, I, O, I ]
bits '6' = [ O, I, I, O ]
bits '7' = [ O, I, I, I ]
bits '8' = [ I, O, O, O ]
bits '9' = [ I, O, O, I ]
bits 'A' = [ I, O, I, O ]
bits 'B' = [ I, O, I, I ]
bits 'C' = [ I, I, O, O ]
bits 'D' = [ I, I, O, I ]
bits 'E' = [ I, I, I, O ]
bits 'F' = [ I, I, I, I ]
bits _ = []

test = do
  describe "tokenize" $ do
    let input = "D2FE28"
    it "tokenizes correctly" $ do
      tokenize input `shouldBe` Seq.fromList [ I, I, O, I, O, O, I, O, I, I, I, I, I, I, I, O, O, O, I, O, I, O, O, O ]
  describe "parseTokens" $ do
    describe "a literal value" $ do
      let tokens = Seq.fromList [ I, I, O, I, O, O, I, O, I, I, I, I, I, I, I, O, O, O, I, O, I, O, O, O ]
      it "parses correctly" $ do
        let Right parsed = parseTokens packetP tokens
        parsed `shouldBe` Packet (Version 6) (Literal 2021)
    describe "an operator, using bit-length" $ do
      let input = "38006F45291200"
          tokens = tokenize input
      it "parses correctly" $ do
        let Right parsed = parseTokens packetP tokens
        parsed `shouldBe` Packet (Version 1) (Operator (operation 6) [Packet (Version 6) (Literal 10)
                                                                     ,Packet (Version 2) (Literal 20)
                                                                     ])
    describe "an operator, using bit-length" $ do
      let input = "EE00D40C823060"
          tokens = tokenize input
      it "parses correctly" $ do
        let Right parsed = parseTokens packetP tokens
        parsed `shouldBe` Packet (Version 7) (Operator (operation 3) [Packet (Version 2) (Literal 1)
                                                                     ,Packet (Version 4) (Literal 2)
                                                                     ,Packet (Version 1) (Literal 3)
                                                                     ])
    describe "an operator, nested" $ do
      let input = "8A004A801A8002F478"
          tokens = tokenize input
          Right parsed = parseTokens packetP tokens
      it "has the right versions" $ do
        versions parsed `shouldMatchList` [Version 4, Version 1, Version 5, Version 6]
      it "parses correctly" $ do
        parsed `shouldBe` Packet (Version 4)
                                 (Operator (operation 2)
                                           (pure (Packet (Version 1)
                                                         (Operator (operation 2)
                                                                   (pure (Packet (Version 5)
                                                                                 (Operator (operation 2)
                                                                                           (pure (Packet (Version 6)
                                                                                                         (Literal 15))))))))))
    describe "pt1" $ forM_
      [("620080001611562C8802118E34", 12), ("C0015000016115A2E0802F182340", 23), ("A0016C880162017C3686B18A3D4780", 31)]
      $ \(input, total) -> it ("handles " <> T.unpack input) $ do
        let parsed = parseOnly (parse input) input
        fmap versionSum parsed `shouldBe` Right total
    describe "pt2" $ do
      let examples = [("C200B40A82", 3)
                     ,("04005AC33890", 54)
                     ,("880086C3E88112", 7)
                     ,("CE00C43D881120", 9)
                     ,("D8005AC2A8F0", 1)
                     ,("F600BC2D8F", 0)
                     ,("9C005AC2F8F0", 0)
                     ,("9C0141080250320F1802104A08", 1)
                     ]
      forM_ examples $ \(input, total) -> it ("handles " <> T.unpack input) $ do
        let parsed = parseOnly (parse input) input
        fmap evaluate parsed `shouldBe` Right total

versions :: Packet -> [Version]
versions (Packet v payload) = v : case payload of Operator _ ps -> ps >>= versions
                                                  _ -> []

versionSum :: Packet -> Int
versionSum = sum . fmap (fromIntegral . getVersion) . versions

evaluate :: Packet -> Word
evaluate p = case packetPayload p of
  Literal v -> v
  Operator op operands -> let values = fmap evaluate operands
                           in case op of SumOp -> sum values
                                         ProductOp -> product values
                                         Minimum -> minimum values
                                         Maximum -> maximum values
                                         GreaterThan -> let [a, b] = values in if a > b then 1 else 0
                                         LessThan -> let [a, b] = values in if a < b then 1 else 0
                                         Equal -> let [a, b] = values in if a == b then 1 else 0

type PacketParser = SP.SeqParser Bit

parseTokens :: PacketParser Packet -> Tokens -> Either (SP.ParseError Bit) Packet
parseTokens = SP.parse

tokenize :: Text -> Tokens
tokenize = Seq.fromList . (bits <=< T.unpack)

parse :: Text -> Parser Packet
parse = either (fail . renderError) pure . parseTokens p . tokenize
  where
    p = packetP <* (many (SP.expect O) >> SP.eos)
    renderError = show

packetP :: PacketParser Packet
packetP = do
  v <- Version <$> word3BE
  pid <- PackageID <$> word3BE
  if pid == PackageID 4
     then Packet v <$> literalP
     else Packet v <$> operatorP pid

operatorP :: PackageID -> PacketParser Payload
operatorP (PackageID pid) = do
  lengthId <- SP.token
  Operator (operation pid) <$> case lengthId of
    I -> numberedPackets
    O -> bitLengthPackets

operation :: Word8 -> Operation
operation 0 = SumOp
operation 1 = ProductOp
operation 2 = Minimum
operation 3 = Maximum
operation 5 = GreaterThan
operation 6 = LessThan
operation 7 = Equal
operation _ = error "unknown operation"

numberedPackets :: PacketParser [Packet]
numberedPackets = do
  numPackets <- fromBits <$> takeBits 11
  count numPackets packetP

bitLengthPackets :: PacketParser [Packet]
bitLengthPackets = do
  numBits <- fromBits <$> takeBits 15
  tokens <- takeBits numBits
  SP.embed $ SP.parse (some packetP <* SP.eos) (Seq.fromList tokens)

takeBits :: Int -> PacketParser [Bit]
takeBits n = count n SP.token

word3BE :: PacketParser Word8
word3BE = fromBits <$> takeBits 3

fromBits :: (Num a, Foldable f) => f Bit -> a
fromBits = F.foldl' (\a b -> a * 2 + if b == I then 1 else 0) 0

literalP :: PacketParser Payload
literalP = fmap Literal $ do
  prefix <- mconcat <$> many (literalChunkP I)
  lastGroup <- literalChunkP O
  pure $ fromBits (prefix <> lastGroup)
  where
    literalChunkP :: Bit -> PacketParser [Bit]
    literalChunkP b = do
      SP.expect b
      takeBits 4

