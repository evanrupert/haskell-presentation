import System.IO
import System.Random


data Card = Card Suit Value
  deriving (Eq)

instance Ord Card where
  compare (Card _ val1) (Card _ val2) = compare val1 val2

instance Show Card where
  show (Card suit val) = (show val) ++ " of " ++ (show suit)

data Suit
  = Hearts
  | Spades
  | Diamonds
  | Clubs
  deriving (Show, Eq, Enum)

data Value
  = Number Int
  | FaceValue Face
  deriving (Eq, Ord)

instance Show Value where
  show (Number n) = show n
  show (FaceValue face) = show face

data Face
  = Jack
  | Queen
  | King
  | Ace
  deriving (Show, Eq, Ord, Enum)

type Decks = ([Card], [Card])

deckHalf :: Int
deckHalf = 26


cards :: [Card]
cards = numberCards ++ faceCards
  where numberCards = [ Card suit (Number n) | suit <- [Hearts ..], n <- [2..10] ]
        faceCards = [ Card suit (FaceValue face) | suit <- [Hearts ..], face <- [Jack ..] ]


shuffleDeck :: StdGen -> [Card] -> [Card]
shuffleDeck gen [] = []
shuffleDeck gen list = (list !! i) : shuffleDeck newGen newList
  where (rand, newGen) = random gen :: (Int, StdGen)
        i = rand `mod` (length list)
        newList = (take i list) ++ (drop (i+1) list)


halfDeck :: [Card] -> Decks
halfDeck deck = ((take deckHalf deck), (drop deckHalf deck))


main :: IO ()
main = do
  gen <- getStdGen
  let decks = halfDeck (shuffleDeck gen cards)
  putStrLn "Starting the game of war"
  printDeckLengths decks
  putStrLn ""
  playGame decks


playGame :: Decks -> IO ()
playGame (_, []) = putStrLn "Player 1 has won the game"
playGame ([], _) = putStrLn "Player 2 has won the game"
playGame decks = do
  newDecks <- playRound decks
  playGame newDecks


playRound :: Decks -> IO Decks
playRound ((p1Card:p1Deck), (p2Card:p2Deck)) = do
  putStrLn ((show p1Card) ++ " vs " ++ (show p2Card))
  let (winner, newDecks) = checkRoundWinner p1Card p2Card (p1Deck, p2Deck) []
  putStrLn ("Player " ++ winner ++ " wins this round")
  printDeckLengths newDecks
  roundDelay
  return newDecks


roundDelay :: IO ()
roundDelay = do
  putStrLn "Press any key to play next round"
  _ <- getChar
  return ()


printDeckLengths :: Decks -> IO ()
printDeckLengths (p1Deck, p2Deck) = do
  putStrLn ("Player 1: " ++ (show (length p1Deck)))
  putStrLn ("Player 2: " ++ (show (length p2Deck)))


checkRoundWinner :: Card -> Card -> Decks -> [Card] -> (String, Decks)
checkRoundWinner p1Card p2Card (p1Deck, p2Deck) cardsToAdd
  | p1Card > p2Card = ("1", (p1Deck ++ [p1Card, p2Card] ++ cardsToAdd, p2Deck))
  | p1Card < p2Card = ("2", (p1Deck, p2Deck ++ [p1Card, p2Card] ++ cardsToAdd))
  | (length p1Deck) < 3 = ("1", ([], p2Deck))
  | (length p2Deck) < 3 = ("2", (p1Deck, []))
checkRoundWinner c1 c2 ((c3:c4:p1Card:deck1), (c5:c6:p2Card:deck2)) cardsToAdd =
  checkRoundWinner p1Card p2Card (deck1, deck2) (cardsToAdd ++ [c1, c2, c3, c4, c5, c6])
