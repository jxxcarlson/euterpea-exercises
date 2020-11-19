-- This compostion is an infinite minimalist chorale in four parts written in Haskell
-- using the Euterpea library.  We also compute the cycle length
-- in beats for each line the music.  With this information in hand,
-- we can compute the cycle length for the infinite composition: the
-- smallest number of beats after which the composition repeats itself.
-- The cycle length is 156780 beats. With this information in hand, we 
-- can construct a one-cycle version of the chorale, export it to Logic Pro,  
-- use better sound files, and export the result to mp3.  In fact, the chorale 
-- is so long that !!we only export the first 200 measures:

-- <audio controls>
--   <source src="/audio/chorale-2.mp3" type="audio/mpeg">
--   Your browser does not support the audio element.
-- </audio>


module Chorale where

import Euterpea
import Lib


-- The composition:

-- Infinite version: 
chorale :: InstrumentName -> Music Pitch
chorale player =  instrument player $ bass :=: tenor :=: tenor2 :=: solo


-- Finite version of the chorale with instrument = clarinet
chorale' :: Music Pitch
chorale' = bassLine' :=: tenorLine' :=: tenorLine2' :=: soloLine'


-- Examples:
-- > playDev 2 $ chorale Bassoon
-- > playDev 2 chorale'


--- Construction of the chorale ---

solo :: Music Pitch
solo = forever $ soloLine

tenor :: Music Pitch
tenor = forever $ tenorLine

tenor2 :: Music Pitch
tenor2 = forever $ tenorLine2

bass :: Music Pitch
bass = forever $ bassLine

-- Compute the length of each line
lSolo = ilength soloLine
lTenor = ilength tenorLine
lTenor2 = ilength tenorLine2
lBass = ilength bassLine

-- Compute the cycle length as the
-- least common multiple of the lengths
-- of the individual lines.
cycleLength = lcm_ [lSolo, lTenor, lTenor2, lBass]

-- Compute the number of times each line of
-- music must be repeated so as to have
-- the given cycle length.
nSolo = cycleLength `div` lSolo
nTenor = cycleLength `div` lTenor
nTenor2 = cycleLength `div` lTenor2
nBass = cycleLength `div` lBass


-- Construct one cycle of the chorale
soloLine', tenorLine', tenorLine2', bassLine' :: Music Pitch
soloLine' = mrepeat nSolo soloLine 
tenorLine' = mrepeat nTenor tenorLine
tenorLine2' = mrepeat nTenor2 tenorLine2
bassLine' = mrepeat nBass bassLine

-- Verify:
-- > map ilength [bassLine', tenorLine', tenorLine2', soloLine']
--   [156780,156780,156780,156780]


-- The bass line
bassNote = d 1 
bassNote' = ds 1
bassNote'' = e 1
bassNote''' = f 1
bassLine = line $ [bassNote 6, rest 3, bassNote' 3, rest 3, bassNote'' 3 
                    , rest 2, bassNote''' 4, rest 1 , bassNote 7, rest 4]

-- The tenor
tenorNote = a 1
tenorNote' = c 2
tenorNote'' = d 2
tenorLine = line $ map (rescale (1/2)) [rest 2, tenorNote 8, rest 3
                  , tenorNote' 4, tenorNote'' 5, rest 4]

-- The second tenor
tenorLine2 = rest 7 :+: f 1 4 :+: c 2 4

-- The solo
soloMotifA = line $ [a 2 1, d 3 1, c 4 2, f 4 hn, rest 2]
soloMotif = line $ [a 2 1, d 3 1, c 3 2, f 3 2, e 3 4, rest 2]
soloMotif' = rest 9 :+: soloMotif :+: transpose 3 soloMotif
soloLine = soloMotif' :+: transpose 7 soloMotif' :+: rest 1

