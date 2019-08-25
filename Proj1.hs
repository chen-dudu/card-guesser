


-- module Proj1 (feedback, initialGuess, nextGuess, GameState) where
module Proj1 (initialGuess) where

import Data.List
import Data.Ord (comparing)
import Card

type GameState = [[Card]]

rankchars = "23456789TJQKA"
suitchars = "CDHS"

initialGuess :: Int -> ([Card], GameState)
initialGuess n
    | n < 1 = error("invalid input!")
    | otherwise = (map stringToCard (zipWith formCard ranks suits), [[]])
        where ranks = initialChooseRank separation rankchars
              suits = [suitchars !! i | i <- [0..n]]
              separation = (length rankchars) `div` (n + 1)


-- nextGuess :: ([Card], GameState) -> (Int, Int, Int, Int, Int) -> ([Card], GameState)



-- feedback :: [Card] -> [Card] -> (Int, Int, Int, Int, Int)
-- feedback target guess = (countMatchCard (sort target) (sort guess),
--                          countLowerRank (sortByRank target) (sortByRank guess),
--                          countMatchRank () (),
--                          countHigherRank (sortByRank target) (sortByRank guess),
--                          countMatchSuit () ())



------------------------------------------------------------------------------

-- helper functions

-- given a list, and an integer indicating the separation width, 
-- this function returns a new list of elements with the specified width
initialChooseRank :: Int -> [t] -> [t]
initialChooseRank _ [] = []
initialChooseRank n lst
 | n < 1 = error("invalid input!")
 | otherwise = [lst !! (i*(n + 1) - 1) | i <- [1..length lst], 
                                         (i*(n + 1) - 1) < length lst]

-- this function takes two characters indicating the rank and suit of a card, and produces a Card
formCard :: Char -> Char -> String
formCard rank suie = rank:suie:[]

-- convert the string representation of a card to the actual Card type
stringToCard :: String -> Card
stringToCard card = read card :: Card


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

-- countMatchRank :: [Card] -> [Card] -> Int
-- only for pattern matching to be exhaustive


countHigherRank :: [Card] -> [Card] -> Int
countHigherRank [] [] = 0
countHigherRank xs ys
    | (compareRank (last xs) (last ys)) > 0 = 1 + countHigherRank (init xs) ys
    | otherwise = 0 
-- only for pattern matching to be exhaustive
countHigherRank _ _ = 0


-- countMatchSuit :: [Card] -> [Card] -> Int
-- only for pattern matching to be exhaustive


-- this function compare the ranks of two cards
-- return -1 if the rank of the first onen is smaller
-- return 1 if the rank of the second one is smaller
-- return 0 if they have the same rank
compareRank :: Card -> Card -> Int
compareRank (Card _ rank1) (Card _ rank2)
    | rank1 < rank2 = -1
    | rank1 == rank2 = 0
    | otherwise = 1


-- sort the card list in ascending order of rank
sortByRank :: [Card] -> [Card]
sortByRank = sortBy (comparing rank)

-- sort the card list in ascending order of suit
sortBySuit :: [Card] -> [Card]
sortBySuit = sortBy (comparing suit)
