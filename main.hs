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

isNotFaceCard = not.isFaceCard

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

checkFaceCardFall :: Deck -> Bool
checkFaceCardFall table
  | (length table >= 2) && value (table !! 1) == Jack  && first1NotFace = True
  | (length table >= 3) && value (table !! 2) == Queen && first2NotFace = True
  | (length table >= 4) && value (table !! 3) == King  && first3NotFace = True
  | (length table >= 5) && value (table !! 4) == Ace   && first4NotFace = True
  | otherwise                                    = False
  where first1NotFace = isNotFaceCard $ head table
        first2NotFace = first1NotFace && (isNotFaceCard $ table !! 1)
        first3NotFace = first2NotFace && (isNotFaceCard $ table !! 2)
        first4NotFace = first3NotFace && (isNotFaceCard $ table !! 3)
    

playRound :: [Deck] -> Deck -> StdGen -> Integer -> Int -> Integer
playRound hands table g roundNo playerNo
  | length (filter (not.null) hands) == 1 =
    roundNo
  | (length table) >= 2 && value (head table) == value (table !! 1) =
    playRound (appendTabletoPlayer rand) [] nextG nextRound rand
  | checkFaceCardFall table == True =
    playRound (appendTabletoPlayer prevPlayer) [] g nextRound prevPlayer
  | null $ hands !! playerNo =
    playRound hands table g roundNo nextPlayer
  | (not.null) (filter isFaceCard $ take 4 table) && isNotFaceCard cardToPlay =
    playRound (removePlayerCard playerNo) (cardToPlay:table) g nextRound playerNo
  | otherwise =
    playRound (removePlayerCard playerNo) (cardToPlay:table) g nextRound nextPlayer
  where n                      = length hands
        (rand,nextG)           = randomR (0,n-1) g
        appendTabletoPlayer pl = replaceIndex hands pl $ (hands !! pl) ++ (reverse table)
        removePlayerCard pl    = replaceIndex hands pl $ tail $ hands !! pl
        cardToPlay             = head $ hands !! playerNo
        prevPlayer             = (playerNo + n-1) `rem` n
        nextRound              = roundNo + 1
        nextPlayer             = (playerNo + 1) `rem` n

