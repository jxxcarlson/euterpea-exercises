module Ch03 where

import Euterpea
import Helper

-- WHOLE TONE SCALE --

wts :: Pitch -> [Music Pitch]
wts p = let f ap = note qn (pitch (absPitch p + ap))
      in map f [0, 2, 4, 6, 8]

wtchord :: Pitch -> [Music Pitch]
wtchord  p =  let f ap = note qn (pitch (absPitch p + ap))
      in map f [0, 4, 8]

-- Example:
-- > playDev 2 $ line $ wts a440

-- Recall that line turns a list of Music a values
-- into a Music a value:
--
-- > :t line
-- line :: [Music a] -> Music a



-- EXERCISE 3.1, PART A

-- Transpose a list of pitches by the integer 
-- given in the first argument:
--
--      f1 :: Int -> [Pitch] -> [Pitch]
--
-- We will use
--
-- absPitch :: Pitch -> AbsPitch
-- pitch :: AbsPitch -> Pitch
--
trans_ :: Int -> Pitch -> Pitch
trans_ k p = 
    pitch . (\ap -> ap + k) . absPitch $ p
    
-- Test:
-- > trans_ 1 a440
-- (As,4)

f1 :: Int -> [Pitch] -> [Pitch]
f1 k = map (trans_ k)


-- Test f1. First, define a function to make
-- a list of pitches for the whole-tone scale
-- beginning on a specific note:
--
wtp :: Pitch -> [Pitch]
wtp p = let f ap = pitch (absPitch p + ap)
      in map f [0, 2, 4, 6, 8]

-- Let's test this: 
--     
-- > wtp a440
-- [(A,4),(B,4),(Cs,5),(Ds,5),(F,5)]


-- And now lets transpose the whole-tone scale
-- to the key of G:
--
-- > f1 (-2) $ wtp a440
-- [(G,4),(A,4),(B,4),(Cs,5),(Ds,5)]





-- EXERCISE 3.1, PART B

-- Design a function
--
--     f2 :: [Dur] -> [Music a]
--
-- that turns a list of durations into a list
-- of rests.

f2 :: [Dur] -> [Music a]
f2 durations =  map (\d -> rest d) durations


-- EXERCISE 3.1, PART C

-- Design a function 

--     f3 :: [Music Pitch] -> [Music Pitch]

-- that replaces each note by a note of half the value,
-- followed by a rest of half the value.  This
-- makes the music "staccato."


staccato :: Music Pitch -> Music Pitch
staccato (Prim (Note d pc)) =  Prim (Note (d/2) pc) :+: rest (d/2)
staccato m = m

f3 :: [Music Pitch] -> [Music Pitch]
f3 ms = map staccato ms

-- Test :
-- > playDev 2 $ line $ f3 $ wts a440



--- NOTES ON SECTION 3.6 ---


-- Functions such as line and chord can 
-- easily be impleented using fold.
-- We join elments of a list with an
-- operator and the help of a neutral
-- element (Monoid!)

line_ :: [Music a] -> Music a
line_ = foldl (:+:) (rest 0)

chord_ :: [Music a] -> Music a
chord_ = foldl (:=:) (rest 0)

-- Test:
-- > playDev 2 $ line_ $ wts a440
-- > playDev 2 $ chord_ $ wtchord a440