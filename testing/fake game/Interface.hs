module Interface (main) where

import Game

import Prelude hiding (catch)
import Data.Maybe
import System.Exit
import System.IO
import Control.Exception

-- | Grabs nth element from the list (removing it from the list)
grab :: Int -> [a] -> (a, [a])
grab n xs = f $ splitAt n xs
          where f (ys, zs) = (head zs, ys ++ tail zs)


maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

parseInt :: String -> Int -> IO Int
parseInt s d = maybe (return d) return (maybeRead s)

-- | Takes a question and a default answer and asks the user for the answer.
getAnswerTo :: String -> Char -> IO (Char)
getAnswerTo q d = 
    do putStr q
       a <- getChar
       if a == '\n'
          then return d
          else putStr "\n" >> return a

-- | Starts a new server by asking for a name.
iStart :: ServerList -> IO ServerList
iStart xs = 
    do putStr "Game name: "
       name <- getLine
       server <- startGame name
       putStrLn $ "Game \"" ++ name ++ "\" started."
       return ((name, server) : xs)

-- | Lists servers.
iList :: ServerList -> IO ()
iList [] = putStrLn "There are no running servers."
iList xs = mapM_ putStrLn [ show i ++ ": " ++ fst x | (i, x) <- zip [1..] xs ]

-- | Kills a game by ID.
iKill :: ServerList -> IO ServerList
iKill [] = putStrLn "... there are no servers to nuke!"
        >> return []
iKill xs =
    (do c <- getAnswerTo "Nuke server number: " '0'
        num <- parseInt [c] 0
        if num > 0 && num <= length xs
           then do let ((name, game), xs') = grab (num - 1) xs
                   stopGame game
                   putStrLn $ "KABOOOM! \"" ++ name ++ "\" is no more."
                   return xs'
           else (putStrLn "Invalid server." >> iKill xs))

-- | Exits the application by throwing an IO exception.
iExit :: ServerList -> IO ServerList
iExit xs =
    do a <- getAnswerTo "Do you really want to exit? [n]: " 'n'
       if a /= 'y' 
          then return xs
          else throwIO ExitSuccess >> return []

-- | Cleans up by killing all threads.
iCleanUp :: ServerList -> IO ()
iCleanUp = mapM_ (stopGame . snd)

--
-- Fires up the application.
--
main :: IO ()
main = 
    do mapM_ (flip hSetBuffering NoBuffering) [stdin, stdout, stderr] -- grr...
       showMenu
       mainLoop []

-- | Main application loop.
mainLoop :: ServerList -> IO ()
mainLoop xs = 
    do c <- getAnswerTo "Your choice: " '0'
       n <- parseInt [c] 0
       xs' <- case n of
                   1 -> iStart xs
                   2 -> iKill xs
                   3 -> iList xs >> return xs
                   _ -> iExit xs
       mainLoop xs'
    `finally` iCleanUp xs

-- A menu of what you can do!
showMenu :: IO ()
showMenu = do
    putStrLn "+--+--+--+--+--+--+--+--+--+--+"
    putStrLn "|  MyS: WC3 server in Haskell  |"
    putStrLn "+--+--+--+--+--+--+--+--+--+--+"
    putStrLn "| 1: Start a new server.      |"
    putStrLn "+ 2: Close down a server.     +"
    putStrLn "| 3: List servers.            |"
    putStrLn "+                             +"
    putStrLn "| _: Exit.                    |"
    putStrLn "+--+--+--+--+--+--+--+--+--+--+"    