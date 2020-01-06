{-# LANGUAGE OverloadedStrings #-}

import Data.List.Split as S
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.List (sortBy)
import Data.Ord (comparing)
import Text.Read

import System.IO
import GHC.IO.Handle
import Data.Char
import Data.Text as T

import System.Process
import System.Directory (doesFileExist, renameFile)

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
                    --on first view, on any following view a 4 or 5 will push to NOT DUE

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

-- splits a deck into ([DUE],[NOT DUE])
splitDeck :: [Card] -> Int -> ([Card],[Card])
splitDeck x n  = (a,b)
 where a = [ c | c <- x, c_dueDate c < n]
       b = [ c | c <- x, c_dueDate c > n]

-- adds x NEW cards to the DUE Stack
toDue' :: ([Card],[Card]) -> Int -> ([Card],[Card])
toDue' (x,[]) _     = (x,[])
toDue' x 0          = x
toDue' (x,(y:ys)) i 
 | c_dueDate y == 9999999999 = toDue' (y:x,ys) (i-1)
 | otherwise                 = toDue' (x,ys++[y]) i

-- loops DUE until all cards have been moved to NOT DUE
cycleDeck :: ([Card],[Card]) -> IO()
cycleDeck ([],_)     = print "All cards done"
cycleDeck ((x:xs),y) = do
 system "clear"
 printFace $ (c_front x) ++ "\n"
 score <- getInt'
 printFace $ (c_back x) ++ "\n\n\nYour Answer: " ++ (show score)
 saveDeck (x:xs,y) "/tmp/testdeck.csv.tmp"
 next <- getInt'
 print next
 t <- round <$> getPOSIXTime
 if score == 5 || (score > 3 && c_seen x == True)
  then cycleDeck (xs, updateCard score t x : y)
  else cycleDeck (xs++[updateCard score t x], y)

--Alternative input function. Requires 'Enter' for input
getInt :: IO Int
getInt = do
 input <- getLine
 let maybeInput = readMaybe input :: Maybe Int
 case maybeInput of
  Nothing                  -> getInt
  Just e | e `elem` [0..5] -> return e
  _                        -> getInt

getInt' :: IO Int
getInt' = do
 mystdin <- hDuplicate stdin
 i <- hGetContents mystdin
 return $ digitToInt $ 
          Prelude.head $ 
          Prelude.dropWhile (\x -> ((< 48) . ord) x || ((> 57) . ord) x ) i

saveDeck :: ([Card],[Card]) -> String -> IO ()
saveDeck k f = do
 writeFile f x
  where x = Prelude.init $ Prelude.unlines $ Prelude.map cardToCsv (fst k ++ snd k)

cardToCsv :: Card -> String
cardToCsv x = c_front x ++ ";" ++ 
              c_back x  ++ ";" ++
              (show $ c_eFactor x) ++ ";" ++
              (show $ c_dueDate x) ++ ";" ++
              (show $ c_interval x) ++ ";" ++
              (show $ c_reviews x)

printFace :: String -> IO ()
printFace x = putStrLn $ unpack $ T.replace "<br>" "\n" $ pack x

mvTmpDeck :: String -> IO ()
mvTmpDeck x = renameFile (x ++ ".tmp") x

main :: IO()
main = do 
 hSetBuffering stdin NoBuffering 
 hSetBuffering stdout NoBuffering
 hSetEcho stdin False

 inp <- readFile "/tmp/testdeck.csv"
 let a = Prelude.map (S.splitOn ";") (S.splitOn "\n" inp)
 t <- round <$> getPOSIXTime
 let b = splitDeck (createStack a) t
 cycleDeck $ toDue' (splitDeck (createStack a) t) 5
 mvTmpDeck "/tmp/testdeck.csv" 
 putStrLn "asdf"
