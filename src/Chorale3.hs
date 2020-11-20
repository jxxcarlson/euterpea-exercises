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


module Chorale3 where

import Euterpea
import Lib


-- The composition:

-- Infinite version: 
chorale :: InstrumentName -> Dur -> Music Pitch
chorale player n =  instrument player $ bass n :=: tenor n :=: tenor2 n :=: solo n

-- Examples:
-- > playDev 2 $ chorale Bassoon


--- Construction of the chorale ---

solo :: Dur -> Music Pitch
solo n = cut n $ forever $ soloLine

tenor ::  Dur -> Music Pitch
tenor n = cut n $ forever $ tenorLine

tenor2 ::  Dur -> Music Pitch
tenor2 n = cut n $ forever $ tenorLine2

bass ::  Dur -> Music Pitch
bass n = cut n $ forever $ bass2 0


-- BASS LINE

bass3 = b1 :+: b2 :+: b3 :+: b4 :+: b1

b1 = bass2 0 :+: rest wn
b2 = transpose 5 $ bass2 2 :+: rest wn
b3 = transpose 7 $ bass2 4 :+: rest wn
b4 = transpose 5 $ bass2 2 :+: rest wn        

---  :+: transpose 5 $ bass2 2 :+: bass2  0

bass2 d = instrument Bassoon $ bass1 :=: (instrument Tuba $ transpose 19 $ offset d $ retro $ bass1)

-- The bass line 
-- bassNote :: Dur -> Music Pitch
bass1 :: Music Pitch
bass1= line $ [d 1 wn, f 1 wn, a 1 wn,  g 1 1, f 1 qn, e 1 qn, d 1 qn, c 1 qn, e 1 hn, d 1 wn]
-- bassLine = line $ [bassNote 6, rest 1, bassNote' 3, rest 3, bassNote'' 3, rest 2, bassNote''' 4, rest 1 , bassNote 7, rest 4]


-- The tenor
tenorLine :: Music Pitch
tenorLine = line $ map (rescale (1/2)) [rest 2, tenorNote 8, rest 3
                  , tenorNote' 4, tenorNote'' 5, rest 4]

tenorNote = a 1
tenorNote' = c 2
tenorNote'' = d 2

-- The second tenor
tenorLine2 :: Music Pitch
tenorLine2 = rest 7 :+: f 1 4 :+: c 2 4

-- The solo
soloLine  :: Music Pitch
soloLine = soloMotif' :+: transpose 7 soloMotif' :+: rest 1

soloMotifA = line $ [a 2 1, d 3 1, c 4 2, f 4 hn, rest 2]
soloMotif = line $ [a 2 1, d 3 1, c 3 2, f 3 2, e 3 4, rest 2]
soloMotif' = rest 9 :+: soloMotif :+: transpose 3 soloMotif

