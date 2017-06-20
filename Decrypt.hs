module Main(main) where

  import System.Environment
  import Data.Bits (xor)
  import System.Random
  import System.IO
  import Data.Char
  import Data.Ord
  import Control.Monad

  main :: IO()
  main = do
    (filename:_) <- getArgs
    input <- readFile filename
    key <- readFile "key.txt"
    let decryptedText = decrypt (map integerFromChar input) (map integerFromChar key) 
    writeFile "plaintext.txt" (map integerToChar decryptedText)

  integerFromChar :: Char -> Integer
  integerFromChar c = toInteger ( ord c )

  integerToChar :: Integer -> Char
  integerToChar i = chr $ fromInteger i

  decrypt :: [Integer] -> [Integer] -> [Integer]
  decrypt s k = zipWith xor s k