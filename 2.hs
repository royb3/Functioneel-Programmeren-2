import Data.Ord
import Data.List
import System.Random

--1st excersize
type Prijs = Integer
type Titel = String
type Auteur = String

data Geschrift = Boek Prijs Titel Auteur
                 deriving (Eq, Show)

--2nd excersize
titelOfBook :: Geschrift -> Titel
titelOfBook (Boek _ t _) = t

instance Ord Geschrift where
  compare = comparing titelOfBook

--3rd excersize
voorbeeldLijst = [(Boek 15 "Learn you a haskell" "Miran Lipovača"), (Boek 3475 "Pogingen iets van het leven te maken" "Hendrik Groen"), (Boek 6000 "De verwarde cavia" "Paulien Cornelisse"), (Boek 40 "De weg" "Michael Puett & Christine Gross-Loh"), (Boek 1499 "365 koolhydraatarme recepten" "Nicola Graimes")]
gesorteerdeVoorbeeldLijst = sort voorbeeldLijst

--4th excersize
data Box a = Box a|
           EmptyBox
           deriving Show

--5th excersize
box :: a -> Box a
box a = Box a

unbox :: Box a -> a
unbox(Box a) = a

boxedSortedExampleList = map box gesorteerdeVoorbeeldLijst
--6th excersize
data Bag a = Bag a |
            EmptyBag
            deriving Show

bag :: a -> Bag a
bag a = Bag a

unbag :: Bag a ->   a
unbag(Bag a) = a

--5th excersize
instance Functor Box where
  fmap f EmptyBox = EmptyBox
  fmap f (Box a) = Box(f a)

instance Functor Bag where
  fmap f EmptyBag = EmptyBag
  fmap f (Bag b) = Bag(f b)

--6th excersize
-- http://stackoverflow.com/questions/9139649/how-to-generate-a-list-which-contains-a-given-number-of-random-numbers-within-a
randomList :: (Int, Int) -> Int -> [Int]
randomList (a, b) c = take c $ randomRs (a, b) gen
  where gen = mkStdGen c

list_of_boxed_numbers =  map box (randomList (1, 10) 10)
bagsInBoxes = map box (map bag voorbeeldLijst)

booksInBox :: [Geschrift] -> [Box Geschrift]
booksInBox a = map box a

--7th excersize
data List x = Empty | List x
  deriving (Eq, Ord, Show)
