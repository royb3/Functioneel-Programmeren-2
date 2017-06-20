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
    g <- newStdGen
    let key = take (length input) (randomRs (33, 127::Integer) g)
    writeFile "key.txt" (map integerToChar key)
    let encryptedText = crypt (map integerFromChar input) key
    writeFile "ciphertext.txt" (map integerToChar encryptedText)

  integerFromChar :: Char -> Integer
  integerFromChar c = toInteger ( ord c )

  integerToChar :: Integer -> Char
  integerToChar i = chr $ fromInteger i

  crypt :: [Integer] -> [Integer] -> [Integer]
  crypt s k = zipWith xor s k