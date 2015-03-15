module Scales where

import Data.List (intercalate)

data Note = C | Cs | D | Ds | E | F | Fs | G | Gs | A | As | B
    deriving (Eq, Ord, Show, Enum)

data Interval = Root | MinorSecond | Second | MinorThird | Third |
                PerfectFourth | DiminishedFifth | PerfectFifth |
                MinorSixth | Sixth | MinorSeventh | Seventh
    deriving (Eq, Ord, Show, Enum)

data Mode = Ionian | Dorian | Phrygian | Lydian | Mixolydian | Aeolian | Locrian
    deriving (Eq, Show, Enum)

type Key = Note

type MarkedList a = [(a, Bool)]

-- Setup

guitarStrings = [E, B, G, D, A, E]
neckDots      = [3, 5, 7, 9]
neckLength    = 13

major = [Root, Second, Third, PerfectFourth, PerfectFifth, Sixth, Seventh]

notes      = cycle [C ..]
intervals  = cycle [Root ..]

-- Actual work

getMarked :: MarkedList a -> [a]
getMarked = map fst . filter snd

marks :: [Bool]
marks = map (`elem` major) intervals

grade :: Mode -> Int
grade m = val m . zip [Ionian ..] . getMarked $ zip [0..] marks
    where   val m = snd . head . filter ((==m) . fst)

chromatic :: Key -> [Note]
chromatic k = dropWhile (/=k) notes

modeMarks :: Mode -> [Bool]
modeMarks m = drop (grade m) $ marks

markNotes :: Key -> Mode -> MarkedList Note
markNotes k m = zip (chromatic k) (modeMarks m)

markIntervals :: Mode -> MarkedList Interval
markIntervals m = zip intervals (modeMarks m)

markString :: Key -> Mode -> Note -> [Bool]
markString k m n = map snd . dropWhile ((/=n) . fst) $ markNotes k m

modeScale :: Key -> Mode -> [(Note, Interval)]
modeScale k m = zip (getMarked $ markNotes k m) (getMarked $ markIntervals m)

allStrings :: Note -> Mode -> [[Bool]]
allStrings k m = map (markString k m) guitarStrings

-- Printing stuff

neckHeader :: String
neckHeader = unwords . take neckLength . cycle . map fret $ [0..11]
    where   fret n | n == 0 = " : "
                   | n `elem` neckDots = " " ++ show n ++ " "
                   | otherwise = "   "

printableString :: [Bool] -> String
printableString = intercalate "|" . map fret . take neckLength
    where   fret True  = " + "
            fret False = "   "

printableScale :: Key -> Mode -> String
printableScale k m = unlines . take (length major) . map foo $ modeScale k m
    where   foo (n, i) = show n ++ " - " ++ show i

wholeNeck :: Key -> Mode -> String
wholeNeck k m = unlines $ neckHeader : map printableString (allStrings k m)

printEverything :: Key -> Mode -> IO ()
printEverything k m = do
    putStrLn $ show k ++ " " ++ show m
    putStrLn $ "-----"
    putStrLn $ printableScale k m
    putStrLn $ wholeNeck k m

main = printEverything C Ionian
