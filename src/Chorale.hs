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



-- NOTE:
--
-- The cycle length of the chorale is 3015 measures,
-- as computed below.  This could easily be made
-- much longer by adding rests so as to make the
-- numbers in the list [67, 12, 15, 36] relatively
-- prime.  For example, lcm_ [67, 13, 15, 37] =
-- 483405, so the cycle length is 483405 beats,
-- or 120851.25 4/4 measures.
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


