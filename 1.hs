import Data.Ord
import Data.List
-- 1st excersize
type Lengte = Double
type Straal = Double
type Breedte = Double
data Kleur = Rood|Blauw|Geel
  deriving (Show, Eq)

data Geofig = Vierkant Lengte Kleur
  |Rechthoek Lengte Breedte Kleur
  |Driehoek Lengte Kleur
  |Cirkel Straal Kleur
  deriving (Show,Eq)

--2nd excersize
square = Vierkant 15.0 Blauw
triangle = Driehoek 20.0 Rood
circle = Cirkel 4.0 Geel
rectangle = Rechthoek 5.0 10.0 Blauw

--3th excersize
oppervlakte :: Geofig -> Double
oppervlakte (Vierkant l _) =  l^2
oppervlakte (Rechthoek l b _) = l * b
oppervlakte (Cirkel s _) = s ^ 2 * pi
oppervlakte (Driehoek l _) = l ^ 2 * ((sqrt 3) / 4)

--4th excersize
omtrek :: Geofig -> Double
omtrek (Vierkant l _ ) =  l * 4
omtrek (Rechthoek l b _) = 2*l + 2*b
omtrek (Driehoek l _) =  3 * l
omtrek (Cirkel s _) = s * 2 * pi

--5th excersize
exampleList = [square, circle, triangle, square, circle, square, rectangle]

geofigToString :: Geofig -> String
geofigToString (Vierkant _ _) = "Vierkant"
geofigToString (Rechthoek _ _ _) = "Rechthoek"
geofigToString (Driehoek _ _) = "Driehoek"
geofigToString (Cirkel _ _) = "Cirkel"

getVierkantenUitList :: [Geofig] -> [Geofig]
getVierkantenUitList figs = [a | a <- figs, geofigToString a == "Vierkant"]

--6tst excersize
getGeofigsByName :: [Geofig] -> String -> [Geofig]
getGeofigsByName figs signature = [a | a <- figs, geofigToString a == signature]

--7th excersize
colorToString :: Kleur -> String
colorToString (Blauw) = "Blauw"
colorToString (Rood) = "Rood"
colorToString (Geel) = "Geel"

colorOfGeofig :: Geofig -> Kleur
colorOfGeofig (Vierkant _ kleur) = kleur
colorOfGeofig (Rechthoek _ _ kleur) = kleur
colorOfGeofig (Driehoek _ kleur) = kleur
colorOfGeofig (Cirkel _ kleur) = kleur

getGeofigsByColor :: [Geofig] -> String -> [Geofig]
getGeofigsByColor figs signature = [a | a <- figs, signature == colorToString (colorOfGeofig a)]

--8th excersize
instance Ord Geofig where
  compare = comparing oppervlakte

getSmallestGeofig :: [Geofig] -> Geofig
getSmallestGeofig figs =  head (sort figs)

getLargestGeofig :: [Geofig] -> Geofig
getLargestGeofig figs = last (sort figs)

--9th excersize
addGeofigsToGeofigs :: [Geofig] -> [Geofig] -> [Geofig]
addGeofigsToGeofigs a b = sort (a ++ b)

--10th excersize
getPercentageOfOppervlaktesFromGeofigs :: [Geofig] -> [Double]
getPercentageOfOppervlaktesFromGeofigs figs = let lijstVanOppervlaktes = map oppervlakte figs in
                                              let omsommingVanOppervlaktes = sum lijstVanOppervlaktes in
                                              map (\x -> x / omsommingVanOppervlaktes * 100) lijstVanOppervlaktes
