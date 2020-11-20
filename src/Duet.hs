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


module Duet where

import Euterpea
import Lib


duet = bass3 :=: (offset 6 $ transpose 12 $ bass3) :=: (offset 12 $ ostinato2)

-- OSTINATO

ostinato1 = instrument Trombone $ sta 0.9 $  line [a 3 hn, d 3 hn, c 3 hn,  g 2 hn, e 3 hn, d 3 wn, rest wn]
ostinato2 = mSeq [0, 2, 7, 5, 0] ostinato1 :+: (instrument Trombone $ dim 0.5 $ transpose (-5) $ line [a 3 hn, d 3 hn, c 3 hn,  g 2 hn, e 3 wn, d 3 2])

-- BASS LINE

bass3 = b1 :+: b2 :+: b3 :+: b4 :+: dim 0.6 b1

b1 = bass2 0 :+: rest wn
b2 = transpose 5 $ bass2 2 :+: rest wn
b3 = retro $ transpose 7 $ bass2 4 :+: rest wn
b4 = retro $ transpose 5 $ bass2 2 :+: rest wn        

bass2 d = instrument Bassoon $ bass1 :=: (instrument Tuba $ transpose 19 $ offset d $ retro $ bass1)

bass1 :: Music Pitch
bass1 = line $ [d 1 wn, f 1 wn, a 1 wn,  g 1 1, f 1 qn, e 1 qn, d 1 qn, c 1 qn, e 1 hn, d 1 wn]

