-- lsim - läpsy simulator - Juho Rinta-Paavola 2015
import System.Random
import Data.List

(??) :: [a] -> [Int] -> [a]
xs ?? []     = []
xs ?? (i:is) = (xs !! i):(xs ?? is)

replaceIndex :: [a] -> Int -> a -> [a]
replaceIndex (x:xs) 0 new = new:xs
replaceIndex (x:xs) n new = x:(replaceIndex xs (n-1) new)

data Suit = Club | Diamond | Heart | Spade
          deriving (Read, Show, Enum, Eq, Ord)
data CardValue = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
               deriving (Read, Show, Enum, Eq, Ord)

data Card = Card {value :: CardValue,
                  suit :: Suit}
          deriving (Read, Show, Eq)

instance Ord Card where
  compare c1 c2 = compare (value c1, suit c1) (value c2, suit c2)

instance Enum Card where
  toEnum n = let (v,s) = n `quotRem` 4 in Card (toEnum v) (toEnum s)
  fromEnum c = fromEnum (value c) * 4 + fromEnum (suit c)

isFaceCard :: Card -> Bool
isFaceCard Card {value=Jack}  = True
isFaceCard Card {value=Queen} = True
isFaceCard Card {value=King}  = True
isFaceCard Card {value=Ace}   = True
isFaceCard _                  = False

type Deck = [Card]

deck :: Deck
deck = [Card value suit | value <- [Two .. Ace], suit <- [Club .. Spade]]

mixedDeck :: Int -> Deck
mixedDeck seed = map toEnum random52
  where random52 = take 52 $ randomRs (0,51) gen
        gen      = mkStdGen seed

deal :: Deck -> Int -> [Deck]
deal deck n
  | n == 1    = [deck]
  | n < 1     = []
  | otherwise = thisHand:(deal (deck \\ thisHand) (n-1))
  where thisHand = deck ?? [0,n .. (length deck)-1]

playRound :: [Deck] -> Deck -> StdGen -> Integer -> Int -> Integer
playRound hands table g roundNo playerNo
  | length (filter (not.null) hands) == 1 = roundNo
  | (length table) >= 2 && value (last table) == value (last $ init table) =
    playRound (appendTabletoPlayer rand) [] nextG nextRound rand
  | otherwise =
    playRound (removePlayerCard playerNo) (table ++ [head $ hands !! playerNo]) g nextRound nextPlayer
  where n                      = length hands
        (rand,nextG)           = randomR (0,n-1) g
        appendTabletoPlayer pl = replaceIndex hands pl $ (hands !! pl) ++ table
        removePlayerCard pl    = replaceIndex hands pl $ tail $ hands !! pl
        nextRound              = roundNo + 1
        nextPlayer             = (playerNo + 1) `rem` n

