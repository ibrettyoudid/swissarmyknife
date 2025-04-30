{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}

module RPS where

import System.Random
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe

import Data.Map qualified as M
import Favs

data Play = Rock | Paper | Scissors deriving (Eq, Ord, Enum, Show, Read)

data Action = Play Play | Exit | Unknown

plays = [Rock .. Scissors]

acts = map Play plays ++ [Exit, Unknown]

data Result = Draw | PlayerWin | ComputerWin | Error deriving Show

data Algo = Winning | Player | Computer deriving Show

actsText = map (map toLower . show) plays ++ ["exit"]

actN text = fromMaybe 4 $ findIndex (text `isPrefixOf`) actsText

readAction text = acts !! actN text

askPlayer = do
    putStrLn "Please type rock, paper, scissors or exit"
    readAction <$> getLine

askComputer = toEnum <$> randomRIO (0, 2) :: IO Play

askComputer1 history = do
    let (lastp, lastc, result, winning) = unzip4 history
    let l = length winning
    if
        | l == 0 -> askComputer
        | l == 1 -> return $ beats $ beats $ head winning
        -- | l == 2 -> return $ beats $ toEnum $ mod (fromEnum (head winning) + difference (head winning) (winning !! 1)) 3
        | l >= 2 -> return $ beats $ snd $ maximum
                [ predict winning lastp
                , predict lastp   lastp
                , predict lastc   lastp
                ]

predict inp out = let
    z = zipWith3 predict1 (tail inp) out inp
    in (sum $ map fst $ filter snd $ zip (iterate (*0.7) 1) $ take 10 $ zipWith (==) (tail z) out, head z)

predict1 inp2 out1 inp1 = toEnum $ mod (fromEnum inp1 + difference out1 inp2) 3

predict2 inp out = let
    --p = M.map (combine (+) 0 . flip zip (iterate (*0.5) 1)) $ multimap $ zip (tail inp) out
    (prob, d) = certainty $ map tflip $ combine (+) 0 $ flip zip (iterate (*0.5) 1) $ zipWith difference out (tail inp)
    pred = toEnum $ (`mod` 3) $ fromEnum (head inp) + d
    in (prob, pred)

predict3 inp out = let
    --p = M.map (combine (+) 0 . flip zip (iterate (*0.5) 1)) $ multimap $ zip (tail inp) out
    pat = M.toList $ M.map (certainty . map tflip . combine (+) 0 . flip zip (iterate (*0.5) 1)) $ multimap $ zip (tail inp) out
    prob = sum $ map (fst . snd) pat
    pred = lookup (head inp) pat
    in case pred of
        Just  j -> (prob, snd j)
        Nothing -> (0, head out)

certainty xys = let 
    xs = map fst xys
    m  = maximum xys
    in (fst m / max 1 (sum xs), snd m)

difference a b = (fromEnum a - fromEnum b + 3) `mod` 3

gameLogic1 player computer =
    case difference player computer of
        0 -> Draw
        1 -> PlayerWin
        2 -> ComputerWin
        _ -> Error

gameLogic player computer = if 
    | player   == computer       -> Draw
    | player   == beats computer -> PlayerWin
    | computer == beats player   -> ComputerWin
    | otherwise                  -> Error
        
beats play = if play == Scissors then Rock else succ play

game history = do
    playerAct <- askPlayer
    computer <- askComputer1 history

    case playerAct of
        Play player -> do
            putStrLn $ "player plays "   ++ show player
            putStrLn $ "computer plays " ++ show computer
            let result = gameLogic player computer
            print result
            let winning = case result of
                    PlayerWin   -> player
                    ComputerWin -> computer
                    Draw        -> player
            game ((player, computer, result, winning):history)
                
        Unknown -> do
            putStrLn "i don't understand"
            game history

        Exit -> return ()

main = game []