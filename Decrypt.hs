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
    cipher <- readFile "cipher.txt"
    key <- readFile "key.txt"
    let plainText = map integerToChar $ crypt (map integerFromChar cipher) (map integerFromChar key)
    writeFile "text.txt" $ show plainText

  integerFromChar :: Char -> Integer
  integerFromChar c = toInteger ( ord c )

  integerToChar :: Integer -> Char
  integerToChar i = chr $ fromInteger i


  crypt :: [Integer] -> [Integer] -> [Integer]
  crypt s k = zipWith xor k s
