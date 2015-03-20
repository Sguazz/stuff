module Scales where

import System.Environment (getArgs)
import Data.List (intercalate, nub)

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

-- Actual work

notes      = cycle [C ..]
intervals  = cycle [Root ..]
modes      = cycle [Ionian ..]

tops :: Eq a => [a] -> [a]
tops = nub . take (length [C ..])

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

scaleNotes :: Key -> Scale -> Mode -> [Note]
scaleNotes k s m = getMarked $ markNotes k s m

scaleIntervals :: Scale -> Mode -> [Interval]
scaleIntervals s m = getMarked $ markIntervals s m

modeScale :: Key -> Scale -> Mode -> [(Note, Interval)]
modeScale k s m = zip (scaleNotes k s m) (scaleIntervals s m)

relatives :: Key -> Scale -> Mode -> [(Note, Mode)]
relatives k s m = inKey . fromTop $ zip ns ms
    where   ns = scaleNotes k Major m
            ms = dropWhile (/=m) modes
            fromTop = dropWhile ((/= Ionian) . snd)
            inKey = filter ((`elem` (tops $ scaleNotes k s m)) . fst)

-- Printing stuff

clear = "\x1b[39m"
bar   = "\x1b[37m" ++ "|"
on    = "\x1b[31m" ++ "x"
off   = "\x1b[34m" ++ "-"
hr    = "---------------"

padWith :: a -> Int -> [a] -> [a]
padWith p n s = s ++ replicate (n - length s) p

pad  = padWith ' '
pad' = padWith ""

guitarNeck :: Key -> Scale -> Mode -> [String]
guitarNeck k s m = paddedHeader : captionedStrings
    where   paddedHeader = pad 3 "" ++ neckHeader
            captionedStrings =  zipWith (++) captions strings
            captions = map (pad 3 . show) guitarStrings
            strings = map (guitarString . markString k s m) guitarStrings

neckHeader :: String
neckHeader = unwords . take neckLength . cycle . map fret $ tops [0..]
    where   fret n | n == 0 = " : "
                   | n `elem` neckDots = " " ++ show n ++ " "
                   | otherwise = "   "

guitarString :: [Bool] -> String
guitarString = (++ clear) . intercalate bar . map fret . take neckLength
    where   fret True  = off ++ on  ++ off
            fret False = off ++ off ++ off

fullScale :: Key -> Scale -> Mode -> [String]
fullScale k s m = column title (modeScale k s m)
    where   title = show k ++ " " ++ show s ++ " " ++ show m

relativeModes :: Key -> Scale -> Mode -> [String]
relativeModes k s m = column title (relatives k s m)
    where   title = "Relative Modes"

columnLayout :: [String] -> [String] -> [String]
columnLayout c1 c2 = zipWith layout (pad' (length c2) c1) (pad' (length c1) c2)
    where   layout l1 l2 = (pad padLength l1) ++ l2
            padLength = (10+) . maximum . map length $ c1

column :: Show a => String -> [(Note, a)] -> [String]
column title list = title : hr : printable
    where   printable = tops . map grade $ list
            grade (n, a) = pad 2 (show n) ++ " - " ++ show a

printEverything :: Key -> Scale -> Mode -> IO ()
printEverything k s m = do
    putStrLn $ unlines $ columnLayout (fullScale k s m) (relativeModes k s m)
    putStrLn $ unlines $ guitarNeck k s m

main = do
    [key, scale, mode] <- getArgs
    printEverything (read key) (read scale) (read mode)
