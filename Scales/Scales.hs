module Scales where

import Data.List (intercalate, nub)
import System.Environment (getArgs)

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
tops = nub . take noteCount

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

scaleGrades :: Scale -> Mode -> [Int]
scaleGrades s m = map intervalGrade (scaleIntervals s m)

-- Intervals stuff

markIntervals :: Scale -> Mode -> MarkedList Interval
markIntervals s m = zip intervals (modeMarks s m)

intervalMarks :: Scale -> [Bool]
intervalMarks s = marks intervals (scale s)

intervalGrade :: Interval -> Int
intervalGrade i = val i $ zip intervals [0..]

-- Modes stuff

modeList :: Mode -> [Mode]
modeList m = dropWhile (/= m) modes

modeMarks :: Scale -> Mode -> [Bool]
modeMarks s m = drop (modeGrade m) (intervalMarks s)

modeGrade :: Mode -> Int
modeGrade m = val m . zip modes . getMarked . zip [0..] $ intervalMarks Major

-- Guitar strings stuff

stringMarks :: Scale -> Mode -> Key -> Note -> [Bool]
stringMarks s m k n = map snd . dropKeys n $ markNotes s m k

-- Putting the pieces together

scaleWithIntervals :: Scale -> Mode -> Key -> Map Note Interval
scaleWithIntervals s m k = zip (scaleNotes s m k) (scaleIntervals s m)

relativeModes :: Mode -> Key -> Map Note Mode
relativeModes m k = dropValues Ionian $ zip (scaleNotes Major m k) (modeList m)

modulations :: Mode -> Key -> Map Note Mode
modulations m k = dropValues Ionian $ zip ns (modeList m)
  where ns = map (head . findScale) [0..]
        findScale g = head $ filter (match g) (allScales Major m)
        match g s = head (drop g s) == k

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

neckPad = pad 3
notePad = pad 2

-- Guitar neck

neckHeader :: String
neckHeader = unwords . take neckLength . cycle . map fret $ tops [0..]
  where fret n | n == 0            = " : "
        fret n | n `elem` neckDots = " " ++ show n ++ " "
        fret _                     = "   "

guitarString :: [Bool] -> String
guitarString = (++ clear) . intercalate bar . map fret . take neckLength
  where fret True  = off ++ on  ++ off
        fret False = off ++ off ++ off

guitarNeck :: Scale -> Mode -> Key -> [String]
guitarNeck s m k = paddedHeader : captionedStrings
  where paddedHeader = neckPad "" ++ neckHeader
        captionedStrings =  zipWith (++) captions strings
        captions = map (neckPad . show) guitarStrings
        strings = map (guitarString . stringMarks s m k) guitarStrings

-- Format columns

scaleColumn :: Scale -> Mode -> Key -> [String]
scaleColumn s m k = column title (scaleWithIntervals s m k)
  where title = show k ++ " " ++ show s ++ " " ++ show m

relativeColumn :: Mode -> Key -> [String]
relativeColumn m k = column title (relativeModes m k)
  where title = "Same as..."

modulationColumn :: Mode -> Key -> [String]
modulationColumn m k = column title (modulations m k)
  where title = "Play these " ++ show m ++ " modes to get..."

-- Such layout very impress wow

columns :: [[String]] -> [String]
columns = foldl1 columnLayout

columnLayout :: [String] -> [String] -> [String]
columnLayout c1 c2 = zipWith layout (pad' (length c2) c1) (pad' (length c1) c2)
  where layout l1 l2 = pad padLength l1 ++ l2
        padLength = (10+) . maximum . map length $ c1

column :: Show a => String -> Map Note a -> [String]
column title list = title : hr : printable
  where printable = tops . map grade $ list
        grade (n, a) = notePad (show n) ++ " - " ++ show a

----------
-- Main --
----------

printEverything :: Scale -> Mode -> Key -> IO ()
printEverything s m k = do
    putStrLn $ unlines $ columns [ scaleColumn s m k
                                 , relativeColumn m k
                                 , modulationColumn m k
                                 ]
    putStrLn $ unlines $ guitarNeck s m k

main = do
    [key, scale, mode] <- getArgs
    printEverything (read scale) (read mode) (read key)
