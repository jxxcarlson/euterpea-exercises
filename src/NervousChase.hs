module NervousChase where

import Euterpea
import Lib

-- Exercise in L-systems per Haskell School of Music
-- Example: playDev 6 $ l2Music 15 40 sn


data DetGrammar a = DetGrammar a
                               [(a, [a])]

    deriving Show

detGenerate :: Eq a => DetGrammar a -> [[a]]
detGenerate (DetGrammar st ps) = iterate (concatMap f) [st]
  where f a = maybe [a] id (lookup a ps)


redAlgae = DetGrammar 'a'
  [('a', "b|c"), ('b', "b"), ('c', "b|d"),
   ('d', "e\\d"), ('e', "f"), ('f', "g"),
   ('g', "h(a)"), ('h', "h"), ('|', "|"),
   ('(', "("), (')', ")"), ('/', "\\"),
   ('\\', "/")]


t n g = sequence_ (map putStrLn (take n (detGenerate g)))

-- > apOp 62 0 qn (note qn (pitch 60))
--   Prim (Note (1 % 4) (D,4)) :+: Prim (Note (1 % 4) (C,4))
apOp :: AbsPitch -> Int  -> Dur -> (Music Pitch -> Music Pitch)
apOp ap shift d =
    \p -> (note d (pitch (ap + shift))) :+: p

charToMusicOp :: AbsPitch -> Dur -> Char -> (Music Pitch -> Music Pitch)
charToMusicOp ap d c = 
    case c of 
        'a' -> apOp ap 0 d
        'b' -> apOp ap 2 d
        'c' -> apOp ap 4 d
        'd' -> apOp ap 5 d
        'e' -> apOp ap 7 d
        'f' -> apOp ap 9 d
        'g' -> apOp ap 11 d
        'h' -> apOp ap 12 d
        '|' -> (\mp -> mp)
        '/' -> (\mp -> mp :+: rest d)
        '\\' -> (\mp -> mp :+: rest (4*d))
        '(' -> (\mp -> transpose 9 mp)
        ')' -> (\mp -> transpose (-9 ) mp)


charToMusicOp1 :: AbsPitch -> Dur -> Char -> (Music Pitch -> Music Pitch)
charToMusicOp1 ap d c = 
    case c of 
        'a' -> apOp ap 0 d
        'b' -> apOp ap 2 d
        'c' -> apOp ap 4 d
        'd' -> apOp ap 5 d
        'e' -> apOp ap 7 d
        'f' -> apOp ap 9 d
        'g' -> apOp ap 11 d
        'h' -> apOp ap 12 d
        '|' -> (\mp -> mp)
        '/' -> (\mp -> mp :+: rest d)
        '\\' -> (\mp -> mp :+: rest d)
        '(' -> (\mp -> transpose 5 mp)
        ')' -> (\mp -> transpose (-4) mp)


strToMusic :: AbsPitch -> Dur -> String -> Music Pitch
strToMusic ap d (c:[]) = charToMusicOp ap d c (rest wn)
strToMusic ap d (c:cs) = charToMusicOp ap d c (strToMusic ap d cs)

lMusic :: Int -> AbsPitch -> Dur -> Music Pitch
lMusic n ap dur = 
    strToMusic ap dur $ mconcat $ take n $ detGenerate redAlgae

l1Music :: Int -> AbsPitch -> Dur -> Music Pitch
l1Music n ap dur = 
    (instrument Oboe $ phrase [Art (Staccato 0.9)] $ lMusic n ap dur) :=: (instrument Bassoon $ lMusic n (ap - 17) (2*dur))

nervousChase :: Int -> AbsPitch -> Dur -> Music Pitch
nervousChase n ap dur = 
      cre 0.4 $ (instrument Xylophone $ phrase [Dyn (Loudness 70)] $ rest 2 :+: lMusic n (ap + 7) (dur))
      :=: (cre 0.4 $ (dim 0.2 $ instrument Bassoon $ phrase [Dyn (Loudness 70), Art (Staccato 0.7)] $ lMusic n ap (dur)))


playL :: Int -> Int -> AbsPitch -> Dur -> IO ()
playL outputId n ap dur = 
   playDev outputId $ lMusic n ap dur -- strToMusic ap dur $ mconcat $ take n $ detGenerate redAlgae
