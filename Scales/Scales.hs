module Scales where

import System.Environment (getArgs)
import Data.List (intercalate)
import Text.Printf (printf)

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

-- Setup

guitarStrings = [E, B, G, D, A, E]
neckDots      = [3, 5, 7, 9]
neckLength    = 13

scale Major      = [Root, Second, Third, PerfectFourth, PerfectFifth, Sixth, Seventh]
scale Pentatonic = [Root, Second, Third, PerfectFifth, Sixth]
scale Blues      = [Root, Second, MinorThird, Third, PerfectFifth, Sixth]

notes      = cycle [C ..]
intervals  = cycle [Root ..]

-- Actual work

getMarked :: MarkedList a -> [a]
getMarked = map fst . filter snd

marks :: Scale -> [Bool]
marks s = map (`elem` scale s) intervals

grade :: Mode -> Int
grade m = val m . zip [Ionian ..] . getMarked $ zip [0..] (marks Major)
    where   val m = snd . head . filter ((==m) . fst)

chromatic :: Key -> [Note]
chromatic k = dropWhile (/=k) notes

modeMarks :: Scale -> Mode -> [Bool]
modeMarks s m = drop (grade m) $ marks s

markNotes :: Key -> Scale -> Mode -> MarkedList Note
markNotes k s m = zip (chromatic k) (modeMarks s m)

markIntervals :: Scale -> Mode -> MarkedList Interval
markIntervals s m = zip intervals (modeMarks s m)

markString :: Key -> Scale -> Mode -> Note -> [Bool]
markString k s m n = map snd . dropWhile ((/=n) . fst) $ markNotes k s m

modeScale :: Key -> Scale -> Mode -> [(Note, Interval)]
modeScale k s m = zip notes intervals
    where   notes     = getMarked $ markNotes k s m
            intervals = getMarked $ markIntervals s m

allStrings :: Note -> Scale -> Mode -> [[Bool]]
allStrings k s m = map (markString k s m) guitarStrings

-- Printing stuff

pad   = printf "%-2v"
clear = "\x1b[39m"
bar   = "\x1b[37m" ++ "|"
on    = "\x1b[31m" ++ "x"
off   = "\x1b[34m" ++ "-"

neckHeader :: String
neckHeader = unwords . take neckLength . cycle . map fret $ [0..11]
    where   fret n | n == 0 = " : "
                   | n `elem` neckDots = " " ++ show n ++ " "
                   | otherwise = "   "

printableString :: (Note, [Bool]) -> String
printableString (n, fs) = pad (show n) ++ string fs ++ clear
    where   string = intercalate bar . map fret . take neckLength
            fret True  = off ++ on  ++ off
            fret False = off ++ off ++ off

printableScale :: Key -> Scale -> Mode -> String
printableScale k s m = unlines . tops . map grade $ modeScale k s m
    where   grade (n, i) = pad (show n) ++ " - " ++ show i
            tops = take (length $ scale s)

wholeNeck :: Key -> Scale -> Mode -> String
wholeNeck k s m = unlines $ paddedHeader : map printableString strings
    where   paddedHeader = pad "" ++ neckHeader
            strings = zip guitarStrings (allStrings k s m)

printEverything :: Key -> Scale -> Mode -> IO ()
printEverything k s m = do
    putStrLn $ show k ++ " " ++ show s ++ " " ++ show m
    putStrLn $ "---------------"
    putStrLn $ printableScale k s m
    putStrLn $ wholeNeck k s m

main = do
    [key, scale, mode] <- getArgs
    printEverything (read key) (read scale) (read mode)
