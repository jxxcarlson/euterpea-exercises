-- An infinite minimalistic chorale in four parts written in Haskell
-- using the Euterpea library.  We also compute the cycle length
-- for the music.

-- The cycle length of the chorale is 3015 measures, as computed below.  This
-- could easily be made much longer by adding rests so as to make the
-- numbers in the list of individual cycle lengths for
-- bass, tenor, tenor2 and solo (36, 15,12, 67) relatively prime.  For
-- example, the least common multiple of 37, 15, 13, 67 is 483405,
-- so the cycle length is 483405 beats, or 120851.25 4/4 measures.


module Chorale where

import Euterpea


-- The composition:
chorale :: InstrumentName -> Music Pitch
chorale player =  instrument player $ bass :=: tenor :=: tenor2 :=: solo

-- Example:
-- > playDev 2 $ chorale Bassoon

--- Its construction ---

solo :: Music Pitch
solo = forever $ soloLine

tenor :: Music Pitch
tenor = forever $ tenorLine

tenor2 :: Music Pitch
tenor2 = forever $ tenorLine2

bass :: Music Pitch
bass = forever $ bassLine

lSolo = ilength soloLine
lTenor = ilength tenorLine
lTenor2 = ilength tenorLine2
lBass = ilength bassLine

cycleLength = lcm_ [lSolo, lTenor, lTenor2, lBass]
nSolo = cycleLength `div` lSolo
nTenor = cycleLength `div` lTenor
nTenor2 = cycleLength `div` lTenor2
nBass = cycleLength `div` lBass

-- Repeat music m n times
mrepeat :: Int -> Music a -> Music a
mrepeat 0 m = rest 0
mrepeat n m = m :+: mrepeat (n - 1) m

soloLine', tenorLine', tenorLine2', bassLine' :: Music Pitch
soloLine' = mrepeat nSolo soloLine 
tenorLine' = mrepeat nTenor tenorLine
tenorLine2' = mrepeat nTenor2 tenorLine2
bassLine' = mrepeat nBass bassLine

-- Verify:
-- > map ilength [bassLine', tenorLine', tenorLine2', soloLine']
--   [156780,156780,156780,156780]

chorale' = bassLine' :=: tenorLine' :=: tenorLine2' :=: soloLine'

bassNote = d 1 
bassNote' = ds 1
bassNote'' = e 1
bassNote''' = f 1
bassLine = line $ [bassNote 6, rest 3, bassNote' 3, rest 3, bassNote'' 3 
                    , rest 2, bassNote''' 4, rest 1 , bassNote 7, rest 4]

tenorNote = a 1
tenorNote' = c 2
tenorNote'' = d 2
tenorLine = line $ map (rescale (1/2)) [rest 2, tenorNote 8, rest 3
                  , tenorNote' 4, tenorNote'' 5, rest 4]

tenorLine2 = rest 7 :+: f 1 4 :+: c 2 4

bigRest :: Music Pitch
bigRest = rest 9

soloMotifA = line $ [a 2 1, d 3 1, c 4 2, f 4 hn, rest 2]
soloMotif = line $ [a 2 1, d 3 1, c 3 2, f 3 2, e 3 4, rest 2]
soloMotif' = rest 9 :+: soloMotif :+: transpose 3 soloMotif
soloLine = soloMotif' :+: transpose 7 soloMotif' :+: rest 1



-- COMPUTATION OF THE CYCLE LENGTH
--
-- > mlength soloLine
-- 67 % 1
-- > mlength tenorLine
-- 13 % 1
-- > mlength tenorLine2
-- 15 % 1
-- > mlength bassLine
-- 36 % 1
-- > lcm_ [67, 12, 15, 36]
-- 12060
-- > 12060 `div` 4
-- 3015

mlength :: Music Pitch -> Dur
mlength mp =
    case mp of 
        (Prim (Note d p)) -> d
        Prim (Rest d) -> d
        m1 :+: m2 -> mlength m1 + mlength m2
        m1 :=: m2 -> maximum [mlength m1, mlength m2]
        Modify control m -> mlength m

ilength :: Music Pitch -> Int 
ilength mp = round $ mlength mp

lcm_ :: [Int] -> Int
lcm_ [] = 1
lcm_ (x:[]) = x
lcm_ (x:xs) = lcm x (lcm_ xs)


-- not used right now:
rescale :: Dur -> Music Pitch -> Music Pitch
rescale factor mp =
    case mp of 
        (Prim (Note d p)) -> Prim (Note (factor * d) p)
        Prim (Rest d) -> Prim (Rest (factor * d))
        m1 :+: m2 -> rescale factor m1 :+: rescale factor m2
        m1 :=: m2 -> rescale factor m1 :=: rescale factor m2
        Modify control m -> Modify control (rescale factor m)


