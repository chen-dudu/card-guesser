


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
nextGuess (guess, oldState) oldFeedback = (newState !! 0, newState) 
    where newState = [i | i <- oldState, (feedback i guess) == oldFeedback]


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

-- countMatchRank :: [Card] -> [Card] -> Int
-- countMatchRank [] [] = 0
-- countMatchRank (x:xs) (y:ys)
--  | equalRank x y = 1 + countMatchRank xs ys
--  | otherwise = countMatchRank xs ys
-- countMatchRank _ _ = 0

countHigherRank :: [Card] -> [Card] -> Int
countHigherRank [] [] = 0
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

-- countMatchSuit :: [Card] -> [Card] -> Int
-- countMatchSuit [] [] = 0
-- countMatchSuit (x:xs) (y:ys)
--  | equalSuit x y = 1 + countMatchSuit xs ys
--  | otherwise = countMatchSuit xs ys
-- countMatchSuit _ _ = 0


getIndex :: Maybe Int -> Int
getIndex Nothing = -1
getIndex (Just n) = n

choose2 :: [Card] -> [[Card]]
choose2 lst = [[i, j] | i <- lst, 
                        j <- (drop (1 + getIndex (elemIndex i lst)) lst)]

choose3 :: [Card] -> [[Card]]
choose3 lst = [[i, j] | i <- lst, 
                        j <- (drop (1 + getIndex (elemIndex i lst)) lst), 
                        k <- (drop (1 + getIndex (elemIndex j lst)) lst)]

choose4 :: [Card] -> [[Card]]
choose4 lst = [[i, j] | i <- lst, 
                        j <- (drop (1 + getIndex (elemIndex i lst)) lst), 
                        k <- (drop (1 + getIndex (elemIndex j lst)) lst), 
                        m <- (drop (1 + getIndex (elemIndex k lst)) lst)]



equalFeedback :: (Int, Int, Int, Int, Int) -> [Card] -> [Card] -> Bool
equalFeedback oldFeedback prevGuess newGuess = 
    (feedback prevGuess newGuess) == oldFeedback



allpos = initialGameState 2 (stringToCard [i:j:[] | j <- suitchars, i <- rankchars])
ans = stringToCard ["2C", "3C"]
guess1 = stringToCard ["6C", "JD"]
f1 = (0,2,0,0,1)
guess2 = stringToCard ["QC", "KC"]  
f2 = (0,2,0,0,2)

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
equalRank :: Card -> Card -> Bool
equalRank (Card _ rank1) (Card _ rank2) = rank1 == rank2

-- this function checks if two cards have the same suit
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

-- sort the card list in ascending order of suit
sortBySuit :: [Card] -> [Card]
sortBySuit = sortBy (comparing suit)

-- similar to the delete funciton defined in Data.List, but based on rank
deleteByRank :: Card -> [Card] -> [Card]
deleteByRank = deleteBy equalRank

-- similar to the delete funciton defined in Data.List, but based on suit
deleteBySuit :: Card -> [Card] -> [Card]
deleteBySuit = deleteBy equalSuit

