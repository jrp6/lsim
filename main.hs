-- lsim - läpsy simulator - Juho Rinta-Paavola 2015
import System.Random
import Data.List

(??) :: [a] -> [Int] -> [a]
xs ?? []     = []
xs ?? (i:is) = (xs !! i):(xs ?? is)

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
  toEnum n = let (v,s) = n `divMod` 4 in Card (toEnum v) (toEnum s)
  fromEnum c = fromEnum (value c) * 4 + fromEnum (suit c)

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

