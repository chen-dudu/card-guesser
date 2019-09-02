module Proj1 (feedback, initialGuess, nextGuess, GameState) where

import Data.List
import Data.Ord (comparing)
import Card

rankchars = "23456789TJQKA"
suitchars = "CDHS"

type GameState = [[Card]]

feedback :: [Card] -> [Card] -> (Int, Int, Int, Int, Int)
feedback target guess = (countMatchCard (sort target) (sort guess),
                         countLowerRank (sortByRank target) (sortByRank guess),
                         countMatchRank target guess,
                         countHigherRank (sortByRank target) (sortByRank guess),
                         countMatchSuit target guess)

initialGuess :: Int -> ([Card], GameState)
initialGuess n
    | n < 1 = error("invalid input!")
    | otherwise = (firstGuess, delete firstGuess (initialGameState n deck))
        where firstGuess = stringToCard (zipWith formCardString ranks suits)
              ranks = initialChooseRank separation rankchars
              suits = [suitchars !! i | i <- [0..n]]
              separation = (length rankchars) `div` (n + 1)
              deck = stringToCard [i:j:[] | j <- suitchars, i <- rankchars]


nextGuess :: ([Card], GameState) -> (Int, Int, Int, Int, Int) -> ([Card], GameState)
nextGuess (oldGuess, oldState) oldFeedback = (newGuess, newState)
    where possibleAns = [i | i <- oldState, (feedback i oldGuess) == oldFeedback]
          newGuess = chooseNextGuess(possibleAns)
          newState = delete newGuess possibleAns


chooseNextGuess :: [[Card]] -> [Card]
chooseNextGuess lst
    | length lst >= 2000 = head lst
    | otherwise = newGuess 
        where feedbackList = [feedback i j | i <- lst, j <- lst]
              scoreList = [(calculateScore(groupByFeedback i lst), i) | i <- lst]
              (_, newGuess) = head(sort scoreList)

------------------------------------------------------------------------------

------------------------------ Helper Functions ------------------------------

------------------------------------------------------------------------------

-- given a list, and an integer indicating the separation width, 
-- this function returns a new list of elements with the specified width
initialChooseRank :: Int -> [t] -> [t]
initialChooseRank _ [] = []
initialChooseRank n lst
 | n < 1 = error("invalid input!")
 | otherwise = [lst !! (i*(n + 1) - 1) | i <- [1..length lst], 
                                         (i*(n + 1) - 1) < length lst]

initialGameState :: Int -> [Card]-> GameState
initialGameState 2 deck = choose2 deck
initialGameState 3 deck = choose3 deck
initialGameState 4 deck = choose4 deck
initialGameState _ _ = error("invalid input!")

-- this function takes two characters indicating 
-- the rank and suit of a card, and produces a String 
-- representation of the Card
formCardString :: Char -> Char -> String
formCardString rank suie = rank:suie:[]

-- input: a list of cards represented as strings
-- output: a list of cards with type Card
stringToCard :: [String] -> [Card]
stringToCard [] = []
stringToCard (x:xs) = (read x :: Card) : stringToCard xs

countMatchCard :: [Card] -> [Card] -> Int
countMatchCard [] [] = 0
countMatchCard (x:xs) (y:ys)
    | x == y = 1 + countMatchCard xs ys
    | otherwise = countMatchCard xs ys
-- only for pattern matching to be exhaustive
countMatchCard _ _ = 0

countLowerRank :: [Card] -> [Card] -> Int
countLowerRank [] [] = 0
countLowerRank (x:xs) (y:ys)
    | (compareRank x y) < 0 = 1 + countLowerRank xs (y:ys)
    | otherwise = 0 
-- only for pattern matching to be exhaustive
countLowerRank _ _ = 0

countMatchRank :: [Card] -> [Card] -> Int
countMatchRank [] [] = 0
countMatchRank (x:xs) ys
    | elemRank x ys = 1 + countMatchRank xs (deleteByRank x ys)
    | otherwise = countMatchRank xs ys
-- only for pattern matching to be exhaustive
countMatchRank _ _ = 0

countHigherRank :: [Card] -> [Card] -> Int
countHigherRank [] [] = 0
countHigherRank [] _ = 0
countHigherRank _ [] = 0
countHigherRank xs ys
    | (compareRank (last xs) (last ys)) > 0 = 1 + countHigherRank (init xs) ys
    | otherwise = 0 
-- only for pattern matching to be exhaustive
countHigherRank _ _ = 0

countMatchSuit :: [Card] -> [Card] -> Int
countMatchSuit [] [] = 0
countMatchSuit (x:xs) ys
    | elemSuit x ys = 1 + countMatchSuit xs (deleteBySuit x ys)
    | otherwise = countMatchSuit xs ys
-- only for pattern matching to be exhaustive
countMatchSuit _ _ = 0

getIndex :: Maybe Int -> Int
getIndex Nothing = -1
getIndex (Just n) = n

choose2 :: [Card] -> [[Card]]
choose2 lst = [[i, j] | i <- lst, 
                        j <- (drop (1 + getIndex (elemIndex i lst)) lst)]

choose3 :: [Card] -> [[Card]]
choose3 lst = [[i, j, k] | i <- lst, 
                        j <- (drop (1 + getIndex (elemIndex i lst)) lst), 
                        k <- (drop (1 + getIndex (elemIndex j lst)) lst)]

choose4 :: [Card] -> [[Card]]
choose4 lst = [[i, j, k, m] | i <- lst, 
                        j <- (drop (1 + getIndex (elemIndex i lst)) lst), 
                        k <- (drop (1 + getIndex (elemIndex j lst)) lst), 
                        m <- (drop (1 + getIndex (elemIndex k lst)) lst)]

------------------------------------------------------------------------------

-- this function compare the ranks of two cards
-- return -1 if the rank of the first onen is smaller
-- return 1 if the rank of the second one is smaller
-- return 0 if they have the same rank
compareRank :: Card -> Card -> Int
compareRank (Card _ rank1) (Card _ rank2)
    | rank1 < rank2 = -1
    | rank1 > rank2 = 1
    | otherwise = 0

-- this function checks if two cards have the same rank
-- used as an equality test function for deleteBy
equalRank :: Card -> Card -> Bool
equalRank (Card _ rank1) (Card _ rank2) = rank1 == rank2

-- this function checks if two cards have the same suit
-- used as an equality test function for deleteBy
equalSuit :: Card -> Card -> Bool
equalSuit (Card suit1 _) (Card suit2 _) = suit1 == suit2

-- use the first card list as benchmark, check if the other two card list
-- have the same feedback
equalFeedback :: [Card] -> [Card] -> [Card] -> Bool
equalFeedback benchmark l1 l2 = (feedback benchmark l1) == (feedback benchmark l2)

-- similar to the elem function defined in the prelude, but based on rank
elemRank :: Card -> [Card] -> Bool
elemRank _ [] = False
elemRank card (x:xs)
    | equalRank card x = True
    | otherwise = elemRank card xs

-- similar to the elem function defined in the prelude, but based on suit
elemSuit :: Card -> [Card] -> Bool
elemSuit _ [] = False
elemSuit card (x:xs)
    | equalSuit card x = True
    | otherwise = elemSuit card xs

-- sort the card list in ascending order of rank
sortByRank :: [Card] -> [Card]
sortByRank = sortBy (comparing rank)

-- sort the card list in ascending order of suit
sortBySuit :: [Card] -> [Card]
sortBySuit = sortBy (comparing suit)

-- similar to the delete funciton defined in Data.List, but based on rank
deleteByRank :: Card -> [Card] -> [Card]
deleteByRank = deleteBy equalRank

-- similar to the delete funciton defined in Data.List, but based on suit
deleteBySuit :: Card -> [Card] -> [Card]
deleteBySuit = deleteBy equalSuit

-- similar to the group function defined in Data.List, but based on feedback
-- with the benchmark
groupByFeedback :: [Card] -> [[Card]] -> [[[Card]]]
groupByFeedback benchmark = groupBy (equalFeedback benchmark)

calculateScore :: [[[Card]]] -> Double
calculateScore [] = 0.0
calculateScore lst = fromIntegral ss / fromIntegral s where (ss, s) = getSums lst

getSums :: [[[Card]]] -> (Int, Int)
getSums [] = (0, 0)
getSums (x:xs) = (ss + (length x)^2, s + length x) where (ss, s) = getSums xs

x = fromIntegral (length benchmark)^2 / fromIntegral (length benchmark)

benchmark = stringToCard ["2C"]
test = [stringToCard ["2C"], stringToCard ["3C"], stringToCard ["3C"], stringToCard ["4C"], stringToCard ["5C"]]