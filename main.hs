{-# LANGUAGE OverloadedStrings #-}

import GHC.IO.Handle (BufferMode(NoBuffering), hSetBuffering, hGetContents, hDuplicate, hSetEcho)
import Data.Text as T (replace, pack, unpack)
import System.Directory (doesFileExist, renameFile)
import Data.Char (ord, digitToInt)
import System.IO (stdin, stdout)
import System.Process (system)
import Control.Monad (when)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.List.Split as S (splitOn)
import Text.Read (readMaybe)
import Data.Ord (comparing)
import Data.List (sortBy)
import System.Environment (getArgs)

data Card = Card
  {
    c_front    :: String,
    c_back     :: String,
    c_eFactor  :: Float,
    c_dueDate  :: Int,
    c_reviews  :: Int,
    c_interval :: Int,
    c_seen  :: Bool --this card was at least answered once in this session
  } deriving (Show) --necessary because cards are only pushed to NOT DUE if 5 is answered
                    --on first view, on any following view a 4 is sufficient to push to NOT DUE

sortByDue :: [Card] -> [Card]
sortByDue = sortBy (comparing c_dueDate)

decodeCard :: [String] -> Maybe Card
decodeCard [a,b,c,d,e,f] = Just $ Card { 
    c_front    = read $ show a :: String, 
    c_back     = read $ show b :: String,
    c_eFactor  = read c        :: Float, 
    c_dueDate  = read d        :: Int, 
    c_reviews  = read e        :: Int, 
    c_interval = read f        :: Int,
    c_seen  = False }
decodeCard _             = Nothing

createStack :: [[String]] -> [Card]
createStack [] = []
createStack (x:xs) = do
  let b = decodeCard x
  case b of
    Just b -> b : createStack xs
    Nothing -> []

--reviews, interval, eFactor
interval :: Int -> Int -> Float -> Int
interval n i ef
 | n == 0 = 1
 | n == 1 = 6
 | otherwise = round $ fromIntegral i * ef

eFactor :: Float -> Int -> Float
eFactor x y 
 | z > 2.5 = 2.5
 | z < 1.3 = 1.3
 | otherwise = z
   where z = x - 0.8 + 0.28 * (fromIntegral y) - 0.02 * (fromIntegral y)**2

updateCard :: Int -> Int -> Card -> Card
updateCard q t c = Card{ c_front = c_front c,
                       c_back = c_back c,
                       c_eFactor = eFactor',
                       c_dueDate = duedate,
                       c_reviews = reviews,
                       c_interval = interval',
                       c_seen = True
                      }
  where eFactor'  = eFactor (c_eFactor c) q
        duedate   = t + interval'*86400 --not c_dueDate c, needs to be 'today' + 
        interval' = interval (c_reviews c) q eFactor'
        reviews
         | q < 3 = 0
         | otherwise = (c_reviews c) + 1

-- splits a deck into ([DUE],[NOT DUE],[NEW])
splitDeck :: [Card] -> Int -> ([Card],[Card],[Card])
splitDeck x n  = (a,b,c)
 where a = [ c | c <- x, c_dueDate c < n, c_dueDate c /= (-1)]
       b = [ c | c <- x, c_dueDate c > n]
       c = [ c | c <- x, c_dueDate c == (-1)]

-- adds x NEW cards to the DUE Stack
toDue :: ([Card],[Card],[Card]) -> Int -> ([Card],[Card],[Card])
toDue (x,y,[])     _ = (x,y,[])
toDue (x,y,z)      0 = (x,y,z)
toDue (x,z,(y:ys)) i = toDue (y:x,z,ys) (i-1)

--Alternative input function. Requires 'Enter' for input
getInt :: IO Int
getInt = do
 input <- getLine
 let maybeInput = readMaybe input :: Maybe Int
 case maybeInput of
  Nothing                  -> getInt
  Just e | e `elem` [1..6] -> return (e-1)
  _                        -> getInt

getInt' :: IO Int
getInt' = do
 mystdin <- hDuplicate stdin
 i <- hGetContents mystdin
 return $ (+) (-1) $ digitToInt $ 
          Prelude.head $ 
          Prelude.dropWhile (\x -> ((< 49) . ord) x || ((> 54) . ord) x ) i

saveDeck :: ([Card],[Card],[Card]) -> String -> IO ()
saveDeck (a,b,c) f = do
 writeFile f x
  where x = Prelude.init $ Prelude.unlines $ Prelude.map cardToCsv (a ++ b ++ c)

cardToCsv :: Card -> String
cardToCsv x = c_front x ++ ";" ++ 
              c_back x  ++ ";" ++
              (show $ c_eFactor x) ++ ";" ++
              (show $ c_dueDate x) ++ ";" ++
              (show $ c_interval x) ++ ";" ++
              (show $ c_reviews x)

printFace :: String -> IO ()
printFace x = putStrLn $ unpack $ T.replace "<br>" "\n" $ pack x

-- probably should include timestamp check
mvSafe :: String -> String -> IO ()
mvSafe src dest = do 
 e <- doesFileExist src
 when e (renameFile src dest)

-- loops DUE until all cards have been moved to NOT DUE
cycleDeck :: ([Card],[Card],[Card]) -> String -> IO()
cycleDeck ([],_,_) _     = print "All cards done"
cycleDeck ((x:xs),y,z) f = do
 system "clear"
 printFace $ (c_front x) ++ "\n"
 reveal <- getInt'
 when ( reveal < 6) (printFace $ (c_back x) ++ "\n\n\n")
 saveDeck (x:xs,y,z) (f++".tmp")
 score <- getInt'
 print score
 t <- round <$> getPOSIXTime
 if score == 5 || (score > 3 && c_seen x == True)
  then cycleDeck (xs, updateCard score t x : y,z) f
  else cycleDeck (xs++[updateCard score t x], y,z) f

study :: String -> IO ()
study deckname = do
 mvSafe (deckname ++ ".tmp") deckname
 inp <- readFile deckname
 t <- round <$> getPOSIXTime
 let a = Prelude.map (S.splitOn ";") (S.splitOn "\n" inp)
 let b = splitDeck (createStack a) t

 cycleDeck ( toDue (splitDeck (createStack a) t) 5 ) deckname
 mvSafe (deckname ++ ".tmp") deckname

addCard :: [String] -> IO ()
addCard [dn,front,back] = do
 mvSafe (dn ++ ".tmp") dn
 inp <- readFile dn
 let a = Prelude.map (S.splitOn ";") (S.splitOn "\n" inp)
 let b = createStack a
 saveDeck ([],[],Card {
    c_front    = front, 
    c_back     = back,
    c_eFactor  = 2.5, 
    c_dueDate  = (-1),
    c_reviews  = 0, 
    c_interval = 0,
    c_seen  = False } : b) (dn ++ ".tmp")


main :: IO()
main = do 
 hSetBuffering stdin NoBuffering 
 hSetBuffering stdout NoBuffering
 hSetEcho stdin False

 -- study "/path/to/deck.csv" "new card qty"
 -- add "/path/to/deck.csv" "c_front" "c_back"
 args <- getArgs
 when (head args == "study") (study $ head $ tail args)
 when (head args == "add") (addCard $ tail args)

 putStrLn "asdf"
