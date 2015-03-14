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

guitarStrings = [E, B, G, D, A, E]
neckDots      = [3, 5, 7, 9]
neckLength    = 13

majorScale = [Root, Second, Third, PerfectFourth, PerfectFifth, Sixth, Seventh]

notes      = cycle [C ..]
intervals  = cycle [Root ..]

neckHeader = unwords . take neckLength . cycle . map fret $ [0..11]
    where   fret n | n == 0 = " : "
                   | n `elem` neckDots = " " ++ show n ++ " "
                   | otherwise = "   "

marks = map (`elem` majorScale) intervals

grade :: Mode -> Int
grade m = val m . zip [Ionian ..] . map snd . filter fst $ zip marks [0..]
    where   val m = snd . head . filter ((==m) . fst)

chromatic :: Key -> [Note]
chromatic k = dropWhile (/=k) notes

modeMarks :: Mode -> [Bool]
modeMarks m = drop (grade m) $ marks

markNotes :: Key -> Mode -> [(Note, Bool)]
markNotes k m = zip (chromatic k) (modeMarks m)

markIntervals :: Mode -> [(Interval, Bool)]
markIntervals m = zip intervals (modeMarks m)

markString :: Key -> Mode -> Note -> [Bool]
markString k m n = map snd . dropWhile ((/=n) . fst) $ markNotes k m

allStrings :: Note -> Mode -> [[Bool]]
allStrings k m = map (markString k m) guitarStrings

getMarked :: [(a, Bool)] -> [a]
getMarked = map fst . filter snd . take (length [C ..])

modeScale :: Key -> Mode -> [Note]
modeScale k m = getMarked $ markNotes k m

modeIntervals :: Mode -> [Interval]
modeIntervals m = getMarked $ markIntervals m

printableString :: [Bool] -> String
printableString = intercalate "|" . map fret . take neckLength
    where   fret True  = " + "
            fret False = "   "

printEverything :: Key -> Mode -> IO ()
printEverything k m = do
    putStrLn $ show k ++ " " ++ show m
    print $ modeScale k m
    print $ modeIntervals m
    putStrLn $ neckHeader
    putStrLn $ unlines $ map printableString (allStrings k m)

main = printEverything A Aeolian
