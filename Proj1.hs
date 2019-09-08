-- File    : Proj1.hs
-- Author  : Liguo Chen
-- Purpose : This file contains the source code for 
--           COMP30020-Declarativev Programming Project 1

module Proj1 (feedback, initialGuess, nextGuess, GameState) where

import Data.List
import Data.Ord (comparing)
import Card

-- Some constants used in this module
rankchars = "23456789TJQKA"
suitchars = "CDHS"

-- The state of the game
type GameState = [[Card]]

-- Given the target as first argument and guess as the second argument,
-- this function calculate the required feedback tuple
feedback :: [Card] -> [Card] -> (Int, Int, Int, Int, Int)
feedback target guess = 
    (countMatchCard (sort target) (sort guess),
    countLowerRank (sortByRank target) (sortByRank guess),
    countMatchRank target guess,
    countHigherRank (sortByRank target) (sortByRank guess),
    countMatchSuit target guess)

-- Given the number of cards in the answer, this function makes 
-- the first guess and setup the state of the game
initialGuess :: Int -> ([Card], GameState)
initialGuess n
    | n < 1 = error("invalid input!")
    | otherwise = (firstGuess, delete firstGuess (initialGameState n deck))
        where firstGuess = stringToCard (zipWith formCardString ranks suits)
              -- rank for the first guess
              ranks = initialChooseRank separation rankchars
              -- suit for the first guess
              suits = [suitchars !! i | i <- [0..n]]
              -- width of gap between cards in the first guess
              separation = (length rankchars) `div` (n + 1)
              -- a standard 52-card deck
              deck = stringToCard [i:j:[] | j <- suitchars, i <- rankchars]

nextGuess :: ([Card], GameState) -> (Int, Int, Int, Int, Int) -> ([Card], GameState)
nextGuess (oldGuess, oldState) oldFeedback = (newGuess, newState)
    where possibleAns = [i | i <- oldState, (feedback i oldGuess) == oldFeedback]
          newGuess = chooseNextGuess possibleAns
          newState = delete newGuess possibleAns

chooseNextGuess :: [[Card]] -> [Card]
chooseNextGuess lst
    | length lst >= 1500 = lst !! (div (length lst) 2)
    | otherwise = newGuess 
        where feedbackList = [feedback i j | i <- lst, j <- lst]
              scoreList = [(calculateScore(group $ sort [feedback j i | j <- lst, j /= i]), i) | i <- lst]
              (_, newGuess) = head(sort scoreList)

------------------------------------------------------------------------------

------------------------------ Helper Functions ------------------------------

------------------------------------------------------------------------------

-- Given two lists of cards, this function returns an integer indicating
-- the number of matching cards in the two input lists
countMatchCard :: [Card] -> [Card] -> Int
countMatchCard [] [] = 0
countMatchCard (x:xs) (y:ys)
    | x == y = 1 + countMatchCard xs ys
    | otherwise = countMatchCard xs ys
-- only for pattern matching to be exhaustive
countMatchCard _ _ = 0

-- Given two lists of cards, this function returns an integer indicating
-- the number of cards in the first input list that have rank lower than
-- the lowest rank in the second input list
countLowerRank :: [Card] -> [Card] -> Int
countLowerRank [] [] = 0
countLowerRank (x:xs) (y:ys)
    -- | (compareRank x y) < 0 = 1 + countLowerRank xs (y:ys)
    | (getRank x) < (getRank y) = 1 + countLowerRank xs (y:ys)
    | otherwise = 0 
-- only for pattern matching to be exhaustive
countLowerRank _ _ = 0

-- Given two lists of cards, this function returns an integer indicating
-- the number of cards with the same rank in the two input lists
countMatchRank :: [Card] -> [Card] -> Int
countMatchRank [] [] = 0
countMatchRank (x:xs) ys
    | elemRank x ys = 1 + countMatchRank xs (deleteByRank x ys)
    | otherwise = countMatchRank xs ys
-- only for pattern matching to be exhaustive
countMatchRank _ _ = 0

-- Given two lists of cards, this function returns an integer indicating
-- the number of cards in the first input list that have rank higher than
-- the highest rank in the second input list
countHigherRank :: [Card] -> [Card] -> Int
countHigherRank [] [] = 0
countHigherRank [] _ = 0
countHigherRank _ [] = 0
countHigherRank xs ys
    -- | (compareRank (last xs) (last ys)) > 0 = 1 + countHigherRank (init xs) ys
    | (getRank (last xs)) < (getRank (last ys)) = 1 + countHigherRank (init xs) ys
    | otherwise = 0

-- Given two lists of cards, this function returns an integer indicating
-- the number of cards with the same suit in the two input lists
countMatchSuit :: [Card] -> [Card] -> Int
countMatchSuit [] [] = 0
countMatchSuit (x:xs) ys
    | elemSuit x ys = 1 + countMatchSuit xs (deleteBySuit x ys)
    | otherwise = countMatchSuit xs ys
-- only for pattern matching to be exhaustive
countMatchSuit _ _ = 0

-- Given an integer indicating the number of cards in the answer, and a list
-- of cards as the deck, this function returns all the combinations
initialGameState :: Int -> [Card]-> GameState
initialGameState 2 deck = choose2 deck
initialGameState 3 deck = choose3 deck
initialGameState 4 deck = choose4 deck
initialGameState _ _ = error("invalid input!")

-- Given a list, and an integer indicating the separation width, 
-- this function returns a new list of elements with the specified width
initialChooseRank :: Int -> [t] -> [t]
initialChooseRank _ [] = []
initialChooseRank n lst
 | n < 1 = error("invalid input!")
 | otherwise = [lst !! (i*(n + 1) - 1) | i <- [1..length lst], 
                                         (i*(n + 1) - 1) < length lst]

-- Given two chars representing the rank and suit of a card,
-- this function returns the string representation of the card
formCardString :: Char -> Char -> String
formCardString rank suie = rank:suie:[]

-- Given a list of strings representing cards, this function converts each
-- card string to the actual Card type.
stringToCard :: [String] -> [Card]
stringToCard [] = []
stringToCard (x:xs) = (read x :: Card) : stringToCard xs

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

-- Given a list of lists of tuples, this function returns the score 
-- for the list using the algorithm provided in the project spec
calculateScore :: [[(Int, Int, Int, Int, Int)]] -> Double
calculateScore [] = 0.0
calculateScore lst = fromIntegral ss / fromIntegral s 
                         where (ss, s) = getSums lst

-- Given a (Maybe Int) type, this funciton returns the integer (-1 for Nothing)
getIndex :: Maybe Int -> Int
getIndex Nothing = -1
getIndex (Just n) = n

-- Given a card, this funciton returns the rank of the card
getRank :: Card -> Rank
getRank (Card suit rank) = rank

-- Given two cards, this function check if they have the same rank
-- this function checks if two cards have the same rank
-- used as an equality test function for deleteBy
equalRank :: Card -> Card -> Bool
equalRank (Card _ rank1) (Card _ rank2) = rank1 == rank2

-- this function checks if two cards have the same suit
-- used as an equality test function for deleteBy
equalSuit :: Card -> Card -> Bool
equalSuit (Card suit1 _) (Card suit2 _) = suit1 == suit2

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

-- similar to the delete funciton defined in Data.List, but based on rank
deleteByRank :: Card -> [Card] -> [Card]
deleteByRank = deleteBy equalRank

-- similar to the delete funciton defined in Data.List, but based on suit
deleteBySuit :: Card -> [Card] -> [Card]
deleteBySuit = deleteBy equalSuit

getSums :: [[(Int, Int, Int, Int, Int)]] -> (Int, Int)
getSums [] = (0, 0)
getSums (x:xs) = (ss + (length x)^2, s + length x) where (ss, s) = getSums xs
