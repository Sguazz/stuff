module Scales where

import Data.List (intercalate)

data Note = C | Cs | D | Ds | E | F | Fs | G | Gs | A | As | B
    deriving (Eq, Ord, Show, Enum)

data Interval = Root | MinorSecond | Second | MinorThird | Third |
                PerfectFourth | DiminishedFifth | PerfectFifth |
                MinorSixth | Sixth | MinorSeventh | Seventh
    deriving (Eq, Ord, Show, Enum)

data Mode = Ionian | Dorian | Phrygian | Lydian | Mixolydian | Aeolian | Locrian
    deriving (Show)

type Key = Note

notes      = cycle [C ..]
intervals  = cycle [Root ..]
majorScale = [Root, Second, Third, PerfectFourth, PerfectFifth, Sixth, Seventh]

grade Ionian     =  0  -- I    (Major)
grade Dorian     =  2  -- II
grade Phrygian   =  4  -- III
grade Lydian     =  5  -- IV
grade Mixolydian =  7  -- V
grade Aeolian    =  9  -- VI   (Minor)
grade Locrian    = 11  -- VII

guitarStrings = [E, B, G, D, A, E]
neckDots      = [3, 5, 7, 9]
neckLength    = 13

marks = map (`elem` majorScale) intervals

chromatic :: Key -> [Note]
chromatic k = dropWhile (/=k) notes

modeScale :: Mode -> [Bool]
modeScale m = drop (grade m) $ marks

markNotes :: Key -> Mode -> [(Note, Bool)]
markNotes k m = zip (chromatic k) (modeScale m)

markIntervals :: Mode -> [(Interval, Bool)]
markIntervals m = zip intervals (modeScale m)

markString :: Key -> Mode -> Note -> [Bool]
markString k m n = map snd . dropWhile ((/=n) . fst) $ markNotes k m

allStrings :: Note -> Mode -> [[Bool]]
allStrings k m = map (markString k m) guitarStrings

getMarked :: [(a, Bool)] -> [a]
getMarked = map fst . filter snd . take (length [C ..])

scale :: Key -> Mode -> [Note]
scale k m = getMarked $ markNotes k m

modeIntervals :: Mode -> [Interval]
modeIntervals m = getMarked $ markIntervals m

printableString :: [Bool] -> String
printableString = intercalate "|" . map fret . take neckLength
    where   fret True  = " + "
            fret False = "   "

neckHeader = unwords . take neckLength . cycle . map fret $ [0..11]
    where   fret n | n == 0 = " : "
                   | n `elem` neckDots = " " ++ show n ++ " "
                   | otherwise = "   "

printGuitarNeck k m = do
    putStrLn $ show k ++ " " ++ show m
    print $ scale k m
    print $ modeIntervals m
    putStrLn $ neckHeader
    putStr $ unlines $ map printableString (allStrings k m)

main = printGuitarNeck A Aeolian
