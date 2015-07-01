module Scales where

import Data.List
import System.Environment

------------------
-- Data & Types --
------------------

data Note = C | Cs | D | Ds | E | F | Fs | G | Gs | A | As | B
    deriving (Eq, Ord, Show, Read, Enum)

data Scale = Major | Pentatonic | Blues
    deriving (Eq, Show, Read)

data Interval = Root       | MinorSecond   | Second          | MinorThird   |
                Third      | PerfectFourth | DiminishedFifth | PerfectFifth |
                MinorSixth | Sixth         | MinorSeventh    | Seventh
    deriving (Eq, Ord, Show, Enum)

data Mode = Ionian | Dorian | Phrygian | Lydian | Mixolydian | Aeolian | Locrian
    deriving (Eq, Show, Read, Enum)

type Key = Note

type MarkedList a = [(a, Bool)]

type Map k v = [(k, v)]

-----------
-- Setup --
-----------

guitarStrings = [E, B, G, D, A, E]
neckDots      = [3, 5, 7, 9]
neckLength    = 13

scale Major      = [Root, Second, Third, PerfectFourth, PerfectFifth, Sixth, Seventh]
scale Pentatonic = [Root, Second, Third, PerfectFifth, Sixth]
scale Blues      = [Root, Second, MinorThird, Third, PerfectFifth, Sixth]

---------------
-- Real work --
---------------

-- Infinite lists are cool

notes      = cycle [C ..]
intervals  = cycle [Root ..]
modes      = cycle [Ionian ..]

noteCount = length [C ..]

tops :: Eq a => [a] -> [a]
tops = take noteCount

-- MarkedList stuff

getMarked :: MarkedList a -> [a]
getMarked = map fst . filter snd

marks :: Eq a => [a] -> [a] -> [Bool]
marks as bs = map (`elem` bs) as

markList :: Eq a => [a] -> [a] -> MarkedList a
markList as bs = zip as (marks as bs)

-- Pretend we have maps and we never ask for bad keys

val :: Eq k => k -> Map k v -> v
val k = snd . head . filter ((== k) . fst)

dropKeys :: Eq k => k -> Map k v -> Map k v
dropKeys k = dropWhile ((/= k) . fst)

dropValues :: Eq v => v -> Map k v -> Map k v
dropValues v = dropWhile ((/= v) . snd)

keys :: Map k v -> [k]
keys = map fst

vals :: Map k v -> [v]
vals = map snd

-- Scales stuff

markNotes :: Scale -> Mode -> Key -> MarkedList Note
markNotes s m k = zip (chromatic k) (modeMarks s m)

chromatic :: Key -> [Note]
chromatic k = dropWhile (/= k) notes

allScales :: Scale -> Mode -> [[Note]]
allScales s m = map (scaleNotes s m) notes

scaleNotes :: Scale -> Mode -> Key -> [Note]
scaleNotes s m k = getMarked $ markNotes s m k

scaleIntervals :: Scale -> Mode -> [Interval]
scaleIntervals s m = getMarked $ markIntervals s m

-- Intervals stuff

markIntervals :: Scale -> Mode -> MarkedList Interval
markIntervals s m = zip intervals (modeMarks s m)

intervalMarks :: Scale -> [Bool]
intervalMarks s = marks intervals (scale s)

-- Modes stuff

modeList :: Mode -> [Mode]
modeList m = dropWhile (/= m) modes

modeMarks :: Scale -> Mode -> [Bool]
modeMarks s m = drop (modeGrade m) (intervalMarks s)

modeGrade :: Mode -> Int
modeGrade m = val m . zip modes . getMarked . zip [0..] $ intervalMarks Major

findModulation :: Mode -> Key -> Mode -> Key
findModulation m k t = head $ drop grade (chromatic k)
  where grade = noteCount + modeGrade m - modeGrade t

-- Guitar strings stuff

stringMarks :: Scale -> Mode -> Key -> Note -> [Bool]
stringMarks s m k n = map snd . dropKeys n $ markNotes s m k

allStrings :: Scale -> Mode -> Key -> [Note] -> [[Bool]]
allStrings s m k = map (stringMarks s m k)

-- Putting the pieces together

scaleWithIntervals :: Scale -> Mode -> Key -> Map Note Interval
scaleWithIntervals s m k = zip (scaleNotes s m k) (scaleIntervals s m)

relativeModes :: Mode -> Key -> Map Note Mode
relativeModes m k = dropValues Ionian $ zip (scaleNotes Major m k) (modeList m)

modulations :: Mode -> Key -> Map Note Mode
modulations m k = dropValues Ionian $ zip ns (modeList m)
  where ns = map (findModulation m k) (modeList m)

--------------------
-- Printing stuff --
--------------------

-- Whatever

clear = "\x1b[39m"
bar   = "\x1b[37m" ++ "|"
on    = "\x1b[31m" ++ "x"
off   = "\x1b[34m" ++ "-"
hr    = "---------------"

padWith :: a -> Int -> [a] -> [a]
padWith p n s = s ++ replicate (n - length s) p

pad  = padWith ' '
pad' = padWith ""

padListWith :: (Int -> [a] -> [a]) -> [[a]] -> [[a]]
padListWith p ss = map (p l) ss
  where l = maximum . map length $ ss

padList  = padListWith pad
padList' = padListWith pad'

-- Such layout very impress wow

mapWithHeader :: (Show k, Show v, Eq k, Eq v) => String -> Map k v -> [String]
mapWithHeader t m = t : hr : showMap (nub . tops $ m)

showMap :: (Show k, Show v) => Map k v -> [String]
showMap m = columnLayout " - " ks vs
  where ks = map show (keys m)
        vs = map show (vals m)

rows :: [[String]] -> [String]
rows = intercalate [""]

columns :: [[String]] -> [String]
columns = foldl1 (columnLayout "     ")

columnLayout :: String -> [String] -> [String] -> [String]
columnLayout s c1 c2 = zipWith layout (padList c1') c2'
  where layout l1 l2 = l1 ++ s ++ l2
        [c1', c2'] = padList' [c1, c2]

-- Guitar neck

guitarNeck :: Scale -> Mode -> Key -> [String]
guitarNeck s m k = columnLayout " " ("" : captions) (neckHeader : strings)
  where captions = map show guitarStrings
        strings = allGuitarStrings $ allStrings s m k guitarStrings

allGuitarStrings :: [[Bool]] -> [String]
allGuitarStrings = map guitarString

guitarString :: [Bool] -> String
guitarString = (++ clear) . intercalate bar . map fret . take neckLength
  where fret True  = off ++ on  ++ off
        fret False = off ++ off ++ off

neckHeader :: String
neckHeader = unwords . take neckLength $ frets
  where frets = cycle . tops . map fret $ [0..]
        fret n | n == 0            = " : "
        fret n | n `elem` neckDots = " " ++ show n ++ " "
        fret _                     = "   "

-- Things we want to see

scaleColumn :: Scale -> Mode -> Key -> [String]
scaleColumn s m k = mapWithHeader title (scaleWithIntervals s m k)
  where title = show k ++ " " ++ show s ++ " " ++ show m

relativeColumn :: Mode -> Key -> [String]
relativeColumn m k = mapWithHeader title (relativeModes m k)
  where title = "Same as..."

modulationColumn :: Mode -> Key -> [String]
modulationColumn m k = mapWithHeader title (modulations m k)
  where title = "Play these " ++ show m ++ " modes to get..."

----------
-- Main --
----------

layout :: Scale -> Mode -> Key -> [String]
layout s m k =
    rows [ columns [ scaleColumn s m k
                   , relativeColumn m k
                   , modulationColumn m k ]
         , guitarNeck s m k ]

display :: [String] -> IO ()
display = putStrLn . unlines

main = do
    [key, scale, mode] <- getArgs
    display $ layout (read scale) (read mode) (read key)
