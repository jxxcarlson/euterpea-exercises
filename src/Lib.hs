module Lib where

import Euterpea



-- Repeat the music m n times
mrepeat :: Int -> Music a -> Music a
mrepeat 0 m = rest 0
mrepeat n m = m :+: mrepeat (n - 1) m

-- Compute the length of a melody in beats
mlength :: Music a -> Dur
mlength mp =
    case mp of 
        (Prim (Note d p)) -> d
        Prim (Rest d) -> d
        m1 :+: m2 -> mlength m1 + mlength m2
        m1 :=: m2 -> maximum [mlength m1, mlength m2]
        Modify control m -> mlength m


-- Compute the length of a melody in beats, but convert
-- to an integer.  Oops, we loose fractional beats,
-- if there are any.
ilength :: Music Pitch -> Int 
ilength mp = round $ mlength mp

-- Least common multiple of a list of integers.
lcm_ :: [Int] -> Int
lcm_ [] = 1
lcm_ (x:[]) = x
lcm_ (x:xs) = lcm x (lcm_ xs)

--Rescale the durations in a piece by a factor
rescale :: Dur -> Music Pitch -> Music Pitch
rescale factor mp =
    case mp of 
        (Prim (Note d p)) -> Prim (Note (factor * d) p)
        Prim (Rest d) -> Prim (Rest (factor * d))
        m1 :+: m2 -> rescale factor m1 :+: rescale factor m2
        m1 :=: m2 -> rescale factor m1 :=: rescale factor m2
        Modify control m -> Modify control (rescale factor m)





perfTime :: Rational -> Music a -> Rational
perfTime metronome x = dur x * (metronome / 60)

pt = perfTime 120



rit r = Modify (Phrase [Tmp $ Ritardando r])
acc a = Modify (Phrase [Tmp $ Accelerando a])
dim d = Modify (Phrase [Dyn $ Diminuendo d])
cre c = Modify (Phrase [Dyn $ Crescendo c])
sta s = Modify (Phrase [Art (Staccato s)])
akk a = Modify (Phrase [Dyn $ Accent a])

prefixes ::       [a] -> [[a]]
prefixes []     = []
prefixes (x:xs) = let f pf = x : pf
                  in [x] : map f (prefixes xs)



dmap :: Dur -> [Dur -> Music Pitch] -> [Music Pitch]
dmap dur pitchOps = applyfuncs pitchOps dur


applyfuncs :: [a -> b] -> a -> [b]
applyfuncs fs a = 
    case fs of
        [] -> []
        (f:fs) -> (f a):(applyfuncs fs a)


--  > mApply [(2*), (3*)] [2, 3]
--   [4,9]
mApply :: [a -> b] -> [a] -> [b]
mApply fs as = 
    let 
        ap (f,a) = f a
     in 
        map ap $ zip fs as  



{-|
   Make a sequence of out a given phrase with given intervals
-}
mSeq :: [Int] -> Music a -> Music a
mSeq [] m = rest 0
mSeq (i:is) m = (transpose i m) :+: (mSeq is m)

unison, mi2, ma2, mi3, ma3, p4, tritone, p5, mi6, ma6, mi7, ma7, octave :: Int
unison = 0
mi2 = 1
ma2 = 2
mi3 = 3
ma3 = 4
p4 = 5
tritone = 6
p5 = 7
mi6 = 8
ma6 = 9
mi7 = 10
ma7 = 11
octave = 12


invertIntervals :: [Int] -> [Int]
invertIntervals is = map (\x -> (-x)) is