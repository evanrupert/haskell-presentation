import System.IO
import System.Random


-- Define Card data type with automatic equality typeclass instance
data Card = Card Suit Value
  deriving (Eq)

-- Create Ord instance in order to see which card is greater
instance Ord Card where
  compare (Card _ val1) (Card _ val2) = compare val1 val2

-- Create show instance to be able to print the card to the screen
instance Show Card where
  show (Card suit val) = (show val) ++ " of " ++ (show suit)

-- Define Suit data type for the card suit
data Suit
  = Hearts
  | Spades
  | Diamonds
  | Clubs
-- Automatic instances of Show, Eq, and Enum typeclasses
-- Use enum in order to create a range of suit values
  deriving (Show, Eq, Enum)

-- Define data type for card value, either number value or face value
data Value
  = Number Int
  | FaceValue Face
  deriving (Eq, Ord)

-- Define Show instance for Value in order to print the card value to the screen
instance Show Value where
  show (Number n) = show n
  show (FaceValue face) = show face

-- Define data type for face values
data Face
  = Jack
  | Queen
  | King
  | Ace
  -- Derive enum in order to use ranges for card generation
  deriving (Show, Eq, Ord, Enum)

-- Define type alias for both player's decks of cards
type Decks = ([Card], [Card])

-- Define constant for half of a deck
deckHalf :: Int
deckHalf = 26


-- Make deck of cards by generating every combination of suit, face, and number
cards :: [Card]
cards = numberCards ++ faceCards
  where numberCards = [ Card suit (Number n) | suit <- [Hearts ..], n <- [2..10] ]
        faceCards = [ Card suit (FaceValue face) | suit <- [Hearts ..], face <- [Jack ..] ]


-- Shuffle deck randomly
shuffleDeck :: StdGen -> [Card] -> [Card]
shuffleDeck gen [] = []
shuffleDeck gen list = (list !! i) : shuffleDeck newGen newList
  where (rand, newGen) = random gen :: (Int, StdGen)
        i = rand `mod` (length list)
        newList = (take i list) ++ (drop (i+1) list)


-- Cut the deck into two even halves
halfDeck :: [Card] -> Decks
halfDeck deck = ((take deckHalf deck), (drop deckHalf deck))


-- Generate the deck and start the game
main :: IO ()
main = do
  gen <- getStdGen
  let decks = halfDeck (shuffleDeck gen cards)
  putStrLn "Starting the game of war"
  printDeckLengths decks
  putStrLn ""
  playGame decks


-- Determine the winner if one deck is empty, otherwise play a round and recurse
playGame :: Decks -> IO ()
playGame (_, []) = putStrLn "Player 1 has won the game"
playGame ([], _) = putStrLn "Player 2 has won the game"
playGame decks = do
  newDecks <- playRound decks
  playGame newDecks


-- Displays the cards of a single round then recurses after winner has been awarded cards
playRound :: Decks -> IO Decks
playRound ((p1Card:p1Deck), (p2Card:p2Deck)) = do
  putStrLn ((show p1Card) ++ " vs " ++ (show p2Card))
  let (winner, newDecks) = checkRoundWinner p1Card p2Card (p1Deck, p2Deck) []
  putStrLn ("Player " ++ winner ++ " wins this round")
  printDeckLengths newDecks
  roundDelay
  return newDecks


-- Delays the game until the user presses any key
roundDelay :: IO ()
roundDelay = do
  putStrLn "Press any key to play next round"
  _ <- getChar
  return ()


-- Prints the lengths of both player's decks
printDeckLengths :: Decks -> IO ()
printDeckLengths (p1Deck, p2Deck) = do
  putStrLn ("Player 1: " ++ (show (length p1Deck)))
  putStrLn ("Player 2: " ++ (show (length p2Deck)))


-- Determines the winner of the round or resolves a war if the cards are tied
checkRoundWinner :: Card -> Card -> Decks -> [Card] -> (String, Decks)
checkRoundWinner p1Card p2Card (p1Deck, p2Deck) cardsToAdd
  | p1Card > p2Card = ("1", (p1Deck ++ [p1Card, p2Card] ++ cardsToAdd, p2Deck))
  | p1Card < p2Card = ("2", (p1Deck, p2Deck ++ [p1Card, p2Card] ++ cardsToAdd))
  | (length p1Deck) < 3 = ("1", ([], p2Deck))
  | (length p2Deck) < 3 = ("2", (p1Deck, []))
checkRoundWinner c1 c2 ((c3:c4:p1Card:deck1), (c5:c6:p2Card:deck2)) cardsToAdd =
  checkRoundWinner p1Card p2Card (deck1, deck2) (cardsToAdd ++ [c1, c2, c3, c4, c5, c6])
