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


-- BASS LINE

bass3 = b1 :+: b2 :+: b3 :+: b4 :+: b1

b1 = bass2 0 :+: rest wn
b2 = transpose 5 $ bass2 2 :+: rest wn
b3 = retro $ transpose 7 $ bass2 4 :+: rest wn
b4 = retro $ transpose 5 $ bass2 2 :+: rest wn        

---  :+: transpose 5 $ bass2 2 :+: bass2  0

bass2 d = instrument Bassoon $ bass1 :=: (instrument Tuba $ transpose 19 $ offset d $ retro $ bass1)

-- The bass line 
-- bassNote :: Dur -> Music Pitch
bass1 :: Music Pitch
bass1= line $ [d 1 wn, f 1 wn, a 1 wn,  g 1 1, f 1 qn, e 1 qn, d 1 qn, c 1 qn, e 1 hn, d 1 wn]
-- bassLine = line $ [bassNote 6, rest 1, bassNote' 3, rest 3, bassNote'' 3, rest 2, bassNote''' 4, rest 1 , bassNote 7, rest 4]




piece = offset 4 $ instrument Clarinet t2 :=: bass3
piece2 = (rescale hn $ bass3) :=: sta 0.5 (offset 2 $ transpose 24  $ (rescale (hn) bass3))
-- (instrument Marimba $ rescale hn $ mx) :=: 


motif = concat [dmap en [f 3,  g 3], [a 3 qn], dmap en [g 3, f 3, e 3, f 3]
                    , dmap qn [g 3, f 3, e 3, c 3], [d 3 hn]]

mx = line (concat (prefixes motif))

t4 = t3 :+: (transpose 7 $ invert t3) :+: (transpose (-5) $ retro t3) 
t3b = (instrument Marimba t2 )  :=: (offset 4 $ transpose 12 $ instrument Xylophone t2)
t3 = instrument Clarinet t2 :=: (offset 4 $ transpose 12 $ instrument Flute t2)
t2 = t1 :+: rest qn :+: transpose 5 t1 :+: transpose 7 t1 :+: rest qn :+: transpose 5 t1  :+: transpose 2 t1
t1 = t0 :+: rest qn :+: (transpose 2 $ invert t0)

finalPiece =  rescale 0.8 $ (offset 4 $ rescale 1 $ t4) :=: bass3 

-- The second tenor
t0 :: Music Pitch
t0 = line $ concat [dmap en [d 3, e 3, f 3, c 4 , bf 3, g 3, a 3]]

rhythm1 = [qn,  hn,  en,  en,  en,  en,  qn,  hn,  hn]  -- where the last qn should be a rest
rhythm2 = [en, en, en, en, qn, qn]
motif1 =  [d 3, a 3, g 3, f 3, e 3, f 3, a 3, g 3, e 3]
motif2 =  [g 3, f 3, e 3, f 3, a 3, g 3]
m1 = sta 0.95 $ line $ mApply motif1 rhythm1
m2 = sta 0.95 $ line $ mApply motif2 rhythm2

invertIntervals :: [Int] -> [Int]
invertIntervals is = map (\x -> (-x)) is


m3 tp = m1 :+: (transpose 3 $ mSeq tp m2) 
m4 tp = m3 tp :+: (retro $ m3 (invertIntervals tp)) 
m5 = instrument Clarinet $ m4 [0,2,5] :=: (transpose 19 $ offset 1 $ instrument Flute $ m4 [0,2,5])


pt = perfTime 120