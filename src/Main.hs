import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import System.Random
import System.Random.Shuffle (shuffle', shuffle)

import Data.Fixed (mod')

-- Geeft de richting van een zet aan.
type Direction = (Int, Int)

-- De status van een kaart. Een kaart kan ofwel zijn kleur 
-- tonen, ofwel zijn achterkant tonen.
data CardStatus = Hidden | Shown deriving(Show, Eq)

-- Een positie op het speelveld.
type Coordinate = (Int, Int)

-- Representatie van een kaart. Een kaart heeft een status, 
-- een kleur en een positie.
type Card = ( Coordinate, Color, CardStatus)

getCoord :: Card -> Coordinate
getCoord (c,_,_) = c

getColor :: Card -> Color
getColor (_,c,_) = c

getStatus :: Card -> CardStatus
getStatus (_,_,s) = s

-- Representatie van het speelveld.
data Board = Board {
    -- Het speelveld is een lijst van kaarten.
    cards :: [Card],
    -- Hou de omgedraaide kaarten tijdens een beurt bij (maximaal 2).
    turned :: [Card],
    -- Hou de huidige geselecteerde kaart bij.
    selector :: Coordinate
}

-- Aantal kaarten op de x-as.
width :: Int
width = 3

-- Aantal kaarten op de y-as.
height :: Int
height = 3

-- De grootte van een kaart.
scaling :: Int
scaling = 150

-- De grootte van de ruimte tussen de kaarten.
cardInset :: Int
cardInset = 10

-- Initiele positie van het Gloss venster.
windowPosition :: (Int, Int)
windowPosition = (200, 200)

-- Seed voor de random generator.
seed :: Int
seed = 45

-- Framerate van het spel.
fps :: Int
fps = 60

-- Bereken het aantal kaarten op het speelveld.
amountOfCards :: Int
amountOfCards   | even n    = n
                | otherwise = n - 1
                where n = width * height

-- Het Gloss venster
window :: Display
window = InWindow "Memory" (width * scaling, height * scaling) windowPosition

-- Het initiele speelveld.
initBoard :: Board
initBoard = Board {
    cards = generateShuffledCards amountOfCards,
    turned = [],
    selector = (0, 0)
}

----------------------------------------------------------------------
-- Vanaf hier zal het nodig zijn om de functies aan te vullen.
-- De functies die je moet aanvullen zijn steeds gemarkeerd met
-- undefined.
----------------------------------------------------------------------

-- De mogelijke richtingen van de selector.
left, right, up, down :: Direction
left  = (-1, 0)
right = (1, 0)
up    = (0, 1)
down  = (0, -1)

-- Controleer of twee kaarten dezelfde kleur hebben.
match :: Card -> Card -> Bool
match card1 card2 = getColor card1 == getColor card2

-- Zoek een kaart binnen een lijst van kaarten op basis van een positie.
-- Wanneer een kaart gevonden is, wordt deze teruggegeven. Anders wordt
-- een error teruggegeven.
find :: Coordinate -> [Card] -> Card
find target cards
    | length a == 1 =head a
    | otherwise = error "Card not found"
    where a = [card | card@(coord,col,stat) <- cards,  coord == target]

-- Geef een permutatie van een gegeven lijst terug.
-- Hint: Kijk zeker eens naar de System.Random en 
--       System.Random.Shuffle bibliotheken.
shuffleList :: [a] -> [a]
shuffleList l = shuffle' l (length l) (mkStdGen seed)

-- Genereer een lijst met n verschillende kleuren.
-- Hint: Je kan gebruikmaken van de generateColor-functie.
generateColors :: Int -> [Color]
generateColors n = [generateColor i| i <- take n (randomRs (0, 360) (mkStdGen seed))]

-- Genereer een lijst van n kaarten (n/2 kleurenparen).
--TODO: coordinaten moeten nog mooi ingevuld worden na shuffle
generateShuffledCards :: Int -> [Card]
generateShuffledCards n = shuffleList [((0,0), generateColors (div n 2) !! div i 2, Hidden) |i<-[0..n-1]]

-- Controleer of een positie op het spelbord een kaart bevat.
hasCard :: Coordinate -> Bool
--check if cards initBoard contains a card on the given coordinate
hasCard (x,y) = (x,y) `elem` [coord | (coord,_,_) <- cards initBoard]

-- Controleer of de selector vanaf een gegeven locatie in een 
-- gegeven richting kan bewegen.
canMove :: Coordinate -> Direction -> Bool
canMove coord direction =hasCard (addcoord coord direction)

-- Beweeg de selector in een gegeven richting.
move :: Board -> Direction -> Board
move board direction = board {selector = addcoord direction (selector board) }

--functie die coordinaten samenvoegt of 
addcoord :: Coordinate -> Coordinate -> Coordinate
addcoord (cx1,cy1) (cx2,cy2)=(cx1 + cx2, cy1 + cy2)

-- Verander de status van een kaart op een gegeven positie 
-- wanneer de posities overeenkomen.
changeCard :: Coordinate -> CardStatus -> Card -> Card
changeCard c s card
    | c == getCoord card = (getCoord card, getColor card, s)
    |otherwise =  card

-- Verander de status van een enkele kaart in een reeks van 
-- kaarten. Deze functie geeft een lijst terug waar de status 
-- van de kaart is aangepast naar `Shown`.
showCard :: Coordinate -> [Card] -> [Card]
showCard target = map (changeCard target Shown )

-- Verander de status van een enkele kaart in een reeks van 
-- kaarten. Deze functie geeft een lijst terug waar de status 
-- van de kaart is aangepast naar `Hidden`.
hideCard :: Coordinate -> [Card] -> [Card]
hideCard target = map (changeCard target Hidden)

-- Draai de kaart op een gegeven positie op het bord om 
-- als deze nog niet eerder werd omgedraaid.
flipCard :: Coordinate -> Board -> Board
flipCard target board
    | target `elem` [coord | (coord,_,_) <- turned board]  = board
    | otherwise = board {cards = showCard target (cards board), turned = turned board ++ [find target (cards board)]}

-- Reset de laatste omgedraaide kaarten terug naar de `Hidden` status.
resetTurned :: Board -> Board
resetTurned board =let last2= drop (length(turned board) -2) (turned board) in
    board{cards = hideCard (getCoord (last last2)) (hideCard (getCoord (head last2)) (cards board)) ,
    turned= []}

-- Bereken het volgende bord op basis van de omgedraaide kaarten.
-- Hint: We hebben de drie gevallen voor deze functie al voorzien.
nextBoard :: Board -> Board
nextBoard b@Board{ turned = [] }         = b
nextBoard b@Board{ turned = [c1] }       = b
nextBoard b@Board{ turned = [c1, c2] }
                    | c1 `match` c2 = b{turned = []}
                    | otherwise = resetTurned b

-- Zet een positie op het bord om naar een positie op het scherm.
-- Hint: hou zeker rekening met het coordinatensysteem van Gloss.
convert :: Int -> Int -> Float
convert location axis =  fromIntegral location * (fromIntegral axis / fromIntegral amountOfCards) + (fromIntegral (2*cardInset + scaling)/2)

-- Render een vierkant met een gegeven kleur en grootte.
renderColoredSquare :: Int -> Color -> Picture
renderColoredSquare size c = undefined

-- Render de selector.
renderSelector :: Coordinate -> Picture
renderSelector coord = blank

-- Render een kaart.
renderCard :: Card -> Picture
renderCard card = blank

-- Render alle kaarten.
renderCards :: [Card] -> Picture
renderCards = undefined

-- Render het speelveld.
render :: Board -> Picture
render board = blank

-- Hulpfunctie die nagaat of een bepaalde toets is ingedrukt.
isKey :: SpecialKey -> Event -> Bool
isKey k1 (EventKey (SpecialKey k2) Down _ _) = k1 == k2
isKey _  _                                   = False

-- Handel alle toetsaanslagen af.
-- Hint: Je kan gebruikmaken van de isKey hulpfunctie.
handleInput :: Event -> Board -> Board
handleInput ev board = initBoard

-- Startpunt
main :: IO ()
main =  play window white fps initBoard render handleInput step

----------------------------------------------------------------------
-- Hieronder staan een aantal hulpfuncties die je kan gebruiken.
----------------------------------------------------------------------

-- Representatie van een HSL-kleur.
type HSL = (Float, Float, Float)

-- Representatie van een RGB-kleur.
type RGB = (Float, Float, Float)

-- Omzetting van de HSL-kleurenruimte naar de RGB-kleurenruimte.
hslToRgb :: HSL -> RGB
hslToRgb (h, s, l) = (r + m, g + m, b + m)
    where
        h' = h / 60
        c = (1 - abs (2 * l - 1)) * s
        x = c * (1 - abs (h' `mod'` 2 - 1))
        m = l - c / 2
        getRGB h | h < 1     = (c, x, 0)
                 | h < 2     = (x, c, 0)
                 | h < 3     = (0, c, x)
                 | h < 4     = (0, x, c)
                 | h < 5     = (x, 0, c)
                 | otherwise = (c, 0, x)
        (r, g, b) = getRGB h'

-- Genereer een kleur op basis van een hue-waarde [0 - 360].
generateColor :: Float -> Color
generateColor hue = makeColor r g b 1
    where (r, g, b) = hslToRgb (hue, 0.5, 0.5)

-- Update het bord in elke stap.
step :: Float -> Board -> Board
step _ b = b
