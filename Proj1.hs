


-- module Proj1 (feedback, initialGuess, nextGuess, GameState) where
module Proj1 (initialGuess) where

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


-- feedback :: [Card] -> [Card] -> (Int, Int, Int, Int, Int)



-- nextGuess :: ([Card], GameState) -> (Int, Int, Int, Int, Int) -> ([Card], GameState)



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

