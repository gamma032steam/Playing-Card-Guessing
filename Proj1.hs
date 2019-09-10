-- Author: Matthew Lui 993333
-- Purpose: Implementation of a card guessing game.

-- A number of cards a randomly picked from a single deck. initialGuess makes
-- an attempt at choosing the same cards. Feedback provides information as
-- to the correctness of the initalGuess, and is used for each subsequent
-- nextGuess until the cards are a complete match. For all functions,
-- we assume that at least one card has been picked.

module Proj1 (feedback, initialGuess, nextGuess, GameState) where

import Card
import Data.List

-- Sample size of possibilities to use when testing which guess will remove
-- the most possibilities.
-- Higher takes longer but generates better guesses.
target_sample = 25

-- Holds information for the guessing algorithm between steps.
-- possible: The guesses of target cards possible at this step
data GameState = GameState {possible::[[Card]]}

instance Show GameState where
    show gs = show $ (length $ possible gs)
     
-- __________________
-- CORE FUNCTIONS
-- __________________

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

-- Takes in a number of target cards then sets up the first guess and GameState
initialGuess :: Int -> ([Card], GameState)
-- Choose from different suits, and ranks 13(n+1) ranks apart
initialGuess n = (distributeSuits n 
                 (map (\x -> toEnum (x-1)::Rank)
                 (uniformDistribute (fromIntegral n) 1 13)),
    -- To start, any combination is possible
                  GameState (cardCombos n))

-- Calculates the best next guess
nextGuess :: ([Card],GameState) -> (Int,Int,Int,Int,Int) -> ([Card],GameState)
nextGuess (guess, (GameState oldPossible)) lastFeedback =
                -- Consider feedback
                let possible = feedbackFilter oldPossible guess lastFeedback
                    bestNext = bestGuess possible in 
                (bestNext, 
                 GameState possible)

-- __________________
-- HELPER FUNCTIONS
-- __________________

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

-- INITIAL GUESS HELPERS

-- Given a list of ranks, create cards spread across all the suits
distributeSuits :: Int -> [Rank] -> [Card]
distributeSuits 0 _ = []
distributeSuits n ranks = let suits = [Club, Spade, Heart, Diamond] in
    (Card (suits!!(n-1)) (ranks!!(n-1))):(distributeSuits (n-1) ranks)


-- NEXT GUESS HELPERS

-- Filters solutions rendered impossible based on the last guesses' feedback
feedbackFilter :: [[Card]] -> [Card] -> (Int, Int, Int, Int, Int) -> [[Card]]
feedbackFilter candidates guess lastFeedback = 
    filter (\x -> feedback x guess == lastFeedback) candidates

-- Calls the appropriate algorithm to find the best guess
bestGuess :: [[Card]] -> [Card]
bestGuess possible = findBest (take target_sample possible) possible Nothing

-- Looks through all possibilities, and chooses the card which will remove
-- the most possible solutions
findBest :: [[Card]] -> [[Card]] -> Maybe ([Card], Int) -> [Card]
-- Initialize the search
findBest possible (currGuess:guesses) Nothing =
    findBest possible guesses (Just(currGuess, scoreGuess possible currGuess))
findBest _ [] (Just (currBest, _)) = currBest
-- Look through each possible guess to find the best
findBest possible (currGuess:guesses) (Just (currBest, bestScore))
    | score < bestScore = findBest possible guesses (Just (currGuess, score))
    | otherwise = findBest possible guesses (Just(currBest, bestScore))
    where score = scoreGuess possible currGuess 

-- Find the number of cards we can't remove for a given guess
scoreGuess :: [[Card]] -> [Card] -> Int
scoreGuess possible guess = average (
    map (\poss -> length ((feedbackFilter possible guess) 
    (feedback poss guess))) possible)


-- __________________
-- GENERAL HELPER FUNCTIONS
-- __________________

-- Averages a list of ints
average :: [Int] -> Int
average list = (sum list) `div` (length list)

-- Distributes k numbers between l and h (h>l), wherein each number has an
-- equal distance to each other and the bounds.
uniformDistribute :: (RealFrac a1, Enum a1, Integral a2) => 
                      a1 -> a1 -> a1 -> [a2]
uniformDistribute n l h = let step = ((h-l+1)/(n+1)) in 
    take (round n) $ map floor [l + n*step | n <- [1..n], l+n*step <= h]

-- Returns the cards common to two lists
commonCards :: [Card] -> [Card] -> [Card] -> [Card]
commonCards target [] correct = correct
commonCards target ((Card suit rank):guesses) correct
    -- Every card is unique, no so need to remove
    | (Card suit rank) `elem` target = 
        commonCards target guesses ((Card suit rank):correct)
    | otherwise = commonCards target guesses correct
    
-- Generates all possible combinations of n cards from a single deck, that is,
-- without duplicate cards
cardCombos :: Int -> [[Card]]
cardCombos 1 = map (\x -> [x]) ([minBound..maxBound]::[Card])
-- Join combinations of n-1 with the set of all possible cards
cardCombos n = [y:x |
                x <- cardCombos (n-1), 
                y <- ([minBound..maxBound]::[Card]),
                -- Prevent duplicates and only add to half of the lists
                not(y `elem` x), y >= x!!0]
