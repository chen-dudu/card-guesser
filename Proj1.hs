-- File    : Proj1.hs
-- Author  : Liguo Chen
-- Purpose : An implementation of the card guessing game

{- | This file contains the source code for 
     COMP30020-Declarativev Programming Project 1. 
     In this file, three important functions in the game are implemented:

     feedback: takes a target as the first argument and a guess as the 
               second argument, each represented as a list of Cards, and 
               returns the five feedback numebrs as a tuple.
     initialGuess: takes the number of cards in the answer as input and
                   returns a pair of an initial guess, which should be
                   a list of the specified number of cards, and a game
                   state.
     nextGuess: takes as input a pair of the previous guess and game 
                state, and the feedback to this guess as a quintuple of
                counts of the correct cards, low ranks, correct ranks, 
                high ranks and correct suits, and returns a pair of the
                next guess and new game state.
-}

module Proj1 (feedback, initialGuess, nextGuess, GameState) where

------------------------------------------------------------------------------
------------------------------ Required Library ------------------------------
------------------------------------------------------------------------------

import Card
import Data.List
import Data.Set (isSubsetOf)
import Data.Set (fromList)
import Data.Ord (comparing)

------------------------------------------------------------------------------
--------------------------------- Constants ----------------------------------
------------------------------------------------------------------------------

-- all ranks in a standard 52-card deck
allRanks = [R2 .. Ace]
-- all suit in a standard 52-card deck
allSuits = [Club .. Spade]
-- the smallest card in the deck (considering both suit and rank)
firstCard = Card Club R2
-- the largest card in the deck (considering both suit and rank)
lastCard = Card Spade Ace

------------------------------------------------------------------------------
------------------------------ Type Declaration ------------------------------
------------------------------------------------------------------------------

-- The state of the game
type GameState = [[Card]]

------------------------------------------------------------------------------
------------------------------- Main Functions -------------------------------
------------------------------------------------------------------------------

feedback :: [Card] -> [Card] -> (Int, Int, Int, Int, Int)
feedback target guess = 
    (countMatchCard (sort target) (sort guess),
    countLowerRank (sortByRank target) (sortByRank guess),
    countMatchRank target guess,
    countHigherRank (sortByRank target) (sortByRank guess),
    countMatchSuit target guess)

initialGuess :: Int -> ([Card], GameState)
initialGuess n
    | n < 1 = error("invalid input!")
    | otherwise = (firstGuess, delete firstGuess (choose n deck))
        where 
            -- width of gap between cards in the first guess
            separation = (length allRanks) `div` (n + 1)
            -- rank for the first guess
            ranks = initialChooseRank separation allRanks
            -- suit for the first guess
            suits = [allSuits !! i | i <- [0..n]]
            -- first guess for the game
            firstGuess = zipWith formCard suits ranks
            -- a standard 52-card deck
            deck = [firstCard..lastCard]

nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
nextGuess (oldGuess, oldState) oldFeedback = (newGuess, newState)
    where
        -- all card lists in the GameState with the 
        -- same feedback could be answer 
        possibleAns = [i | i <- oldState,(feedback i oldGuess) == oldFeedback]
        -- choose the next guess from all possible answers
        newGuess = chooseNextGuess possibleAns
        -- update GameState
        newState = delete newGuess possibleAns

------------------------------------------------------------------------------
------------------------------ Helper Functions ------------------------------
------------------------------------------------------------------------------

---------------------------- calculating feedback ----------------------------

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
    | (getRank x) < (getRank y) = 1 + countLowerRank xs (y:ys)
    -- the lowest rank in the answer is higher than the lowest 
    -- rank in the guess, no more comparison is needed
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
    | (getRank (last xs)) > (getRank (last ys)) 
        = 1 + countHigherRank (init xs) ys
    -- the highest rank in the answer is lower than the highest 
    -- rank in the guessing, no more comparison is needed
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

----------------------------- making first guess -----------------------------

-- Given an integer indicating the number of cards in the answer, and a list
-- of cards as the deck, this function returns all the combinations
choose :: Int -> [Card] -> [[Card]]
choose 1 lst = [[i] | i <- lst]
choose n (x:xs)
    | n < 0 = error("invalid input!")
    | otherwise = [(head i):j | i <- outerList, j <- innerList, 
                                (head i) < (head j)]
        where outerList = [[k] | k <- (init (x:xs))]
              innerList = choose (n - 1) xs

-- Given a list, and an integer indicating the separation width, 
-- this function returns a new list of elements with the specified width
initialChooseRank :: Int -> [t] -> [t]
initialChooseRank _ [] = []
initialChooseRank n lst
 | n < 1 = error("invalid input!")
 | otherwise = [lst !! (i*(n + 1) - 1) | i <- [1..length lst], 
                                         (i*(n + 1) - 1) < length lst]

----------------------------- making next guess ------------------------------

-- Given a list of lists of cards, this function returns a list of card as the
-- guess for the next guess, using the algoritm provided in the project spec
chooseNextGuess :: [[Card]] -> [Card]
chooseNextGuess lst
    | length lst >= 1500 = lst !! (div (length lst) 2)
    | otherwise = newGuess 
        where feedbackList = [feedback i j | i <- lst, j <- lst]
              scoreList = [(calculateScore(group $ sort 
                            [feedback j i | j <- lst, j /= i]), i) | i <- lst]
              (_, newGuess) = head(sort scoreList)

-- Given a list of lists of tuples, this function returns the score 
-- for the list using the algorithm provided in the project spec
-- score = (sum (size of Xi)^2) / (sum (size of Xi)), 
-- Xi is each tuple list in the list.
calculateScore :: [[(Int, Int, Int, Int, Int)]] -> Double
calculateScore [] = 0.0
calculateScore lst = fromIntegral ss / fromIntegral s 
                         where (ss, s) = getSums lst

---------------------------- supporting functions ----------------------------

-- Given the suit and rank of a card, this function returns a card
-- with the specified suit and rank
formCard :: Suit -> Rank -> Card
formCard suit rank = Card suit rank

-- Given a card, this funciton returns the rank of the card
getRank :: Card -> Rank
getRank (Card _ rank) = rank

-- Given a (Maybe Int) type, this funciton returns the integer (-1 for Nothing)
getIndex :: Maybe Int -> Int
getIndex Nothing = -1
getIndex (Just n) = n

-- Given a list of lists of tuples, this function returns a 2-tuple ---
-- the first component is the sum of square of each tuple list
-- the second component is the sum of each tuple list
getSums :: [[(Int, Int, Int, Int, Int)]] -> (Int, Int)
getSums [] = (0, 0)
getSums (x:xs) = (ss + (length x)^2, s + length x) where (ss, s) = getSums xs

-- similar to the elem function defined in the Prelude, but based on rank
elemRank :: Card -> [Card] -> Bool
elemRank _ [] = False
elemRank card (x:xs)
    | equalRank card x = True
    | otherwise = elemRank card xs

-- similar to the elem function defined in the Prelude, but based on suit
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

-- Given two cards, this function check if they have the same rank
-- this function checks if two cards have the same rank
-- used as an equality test function for deleteBy
equalRank :: Card -> Card -> Bool
equalRank (Card _ rank1) (Card _ rank2) = rank1 == rank2

-- this function checks if two cards have the same suit
-- used as an equality test function for deleteBy
equalSuit :: Card -> Card -> Bool
equalSuit (Card suit1 _) (Card suit2 _) = suit1 == suit2
