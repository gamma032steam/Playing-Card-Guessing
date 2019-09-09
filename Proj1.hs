-- Author: Matthew Lui 993333
-- Purpose Complete implementation of a card guessing game.

-- A number of cards a randomly picked from a single deck. initialGuess makes
-- an attempt at choosing the same cards. Feedback provides information as
-- to the correctness of the initalGuess, and is used for each subsequent
-- nextGuess until the cards are a complete match. For all functions,
-- we assume that at least one card has been picked.

--module Proj1 (feedback, initialGuess, nextGuess, GameState) where
module Proj1 where

import Card
import Data.List

-- Explain this datatype!
data GameState = GameState [[Card]]

instance Show GameState where
    show gs = ""
     
-- __________________
-- CORE FUNCTIONS
-- __________________

-- FEEDBACK

-- Takes a target and a guess and returns five feedback numbers
-- In corresponding order, they are: (1) the number correct, (2) cards lower
-- than the lowest rank, (3) cards in the same rank, (4)  cards higher than the
-- highest rank, (5) cards in the same suit
feedback :: [Card] -> [Card] -> (Int,Int,Int,Int,Int)
feedback target guess = (length (commonCards target guess []), 
                        cardsLower target guess,
                        ranksCorrect target guess 0, 
                        cardsHigher target guess, 
                        suitsCorrect target guess 0)
                        
-- FEEDBACK HELPERS
    
-- Returns the number of correct ranks, tracking the total on each call
ranksCorrect :: [Card] -> [Card] -> Int -> Int
ranksCorrect _ [] hits = hits
ranksCorrect target ((Card _ currrank): guesses) hits
    -- Look for a match
    | (length matches) > 0
    -- Delete the matched element to avoid a many-to-one match
    = ranksCorrect (delete (matches !! 0) target) guesses (hits+1)
    | otherwise = ranksCorrect target guesses hits
    where matches = filter (\card -> rank card == currrank) target
    
-- Returns the number of correct suits, tracking the total on each call
suitsCorrect :: [Card] -> [Card] -> Int -> Int
suitsCorrect _ [] hits = hits
suitsCorrect target ((Card currsuit _): guesses) hits
    -- Look for a match
    | (length matches) > 0
    -- Delete the matched element to avoid a many-to-one match
    = suitsCorrect (delete (matches !! 0) target) guesses (hits+1)
    | otherwise = suitsCorrect target guesses hits
    where matches = filter (\card -> suit card == currsuit) target

-- Returns the number of cards from the target less that the lowest rank from
-- the guess
cardsLower :: [Card] -> [Card] -> Int
cardsLower target guess = let minrank = minimum (map (rank) guess) in
    length(filter(\mycard -> (rank mycard) < minrank) target)

-- Returns the number of cards from the target greater than the highest rank 
-- from the guess
cardsHigher :: [Card] -> [Card] -> Int
cardsHigher target guess = let maxrank = maximum (map (rank) guess) in
    length(filter(\mycard -> (rank mycard) > maxrank) target)


-- INITIAL GUESS

-- Takes in a number of target cards then sets up the first guess and GameState
initialGuess :: Int -> ([Card], GameState)
initialGuess n = (take n [Card s R7 | s <- [minBound..maxBound]::[Suit]], 
                  GameState [[]])


-- NEXT GUESS

nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
nextGuess _ _ = ([], 
                GameState [[]])



-- __________________
-- GENERAL HELPER FUNCTIONS
-- __________________

-- Returns the cards common to two lists
commonCards :: [Card] -> [Card] -> [Card] -> [Card]
commonCards target [] correct = correct
commonCards target ((Card suit rank):guesses) correct
    -- Every card is unique, no so need to remove
    | (Card suit rank) `elem` target = 
        commonCards target guesses ((Card suit rank):correct)
    | otherwise = commonCards target guesses correct
    

-- these are currently not used!
-- Finds the minimum rank in a list of cards
minrank :: [Card] -> Rank
minrank cards = minimum (map (rank) cards)

-- Finds the maximum rank in a list of cards
maxrank :: [Card] -> Rank
maxrank cards = maximum (map (rank) cards)
