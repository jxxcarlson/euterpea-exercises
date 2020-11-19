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


module Sonos where

import Euterpea


doh = c
re = d 
mi = e 
fa = f
sol = g
la = a
lay = af
say = bf
si = b


m1 =  forever $ line [la 2 qn, doh 3 qn, mi 3 qn, re 3 qn, sol 3 hn, la 3 hn, sol 3 wn]
m2 =  forever $ (rest hn) :+: (retro m1)
m =   m1 :=: m2


m3 = line $ [la 2 qn, doh 3 qn, mi 4 en, fa 4 en, mi 4 en, re 4 en, sol 4 qn, la 4 qn,  say 4 qn, la 4 qn, sol 4 qn]


