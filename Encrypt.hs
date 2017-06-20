-- main :: IO()
-- main= do
--   input <- readFile "text.txt"
--   key <- randomList (0, 99) 10
--   mapM_ printChar input

module Main(main) where

  import Data.Bits (xor)
  import System.Random
  import System.IO
  import Data.Char
  import Data.Ord
  import Control.Monad

  main :: IO()
  main = do
    input <- readFile "text.txt"
    g <- newStdGen
    let key = take (length input) (randomRs (0, 255::Integer) g)
    writeFile "key.txt" $ show key
    let encryptedText = crypt (map integerFromChar input) key
    writeFile "cipher.txt" $ show encryptedText
    let test = crypt encryptedText key
    writeFile "test.txt" $ show $ map integerToChar test

  integerFromChar :: Char -> Integer
  integerFromChar c = toInteger ( ord c )

  integerToChar :: Integer -> Char
  integerToChar i = chr $ fromInteger i

  crypt :: [Integer] -> [Integer] -> [Integer]
  crypt s k = zipWith xor s k
