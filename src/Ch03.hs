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





--- EXERCISE 3.9 ---

-- Define a function

--    fuse :: [Dur] -> [Dur -> Music a] -> [Music a]

-- that combines a list of durations with a list of notes 
-- lacking durations to create a list of notes, e.g,

-- > fuse [qn, hn, sn] [c 4, d 4, e 4]
--    [c 4 qn, d 4 hn, e 4 sn]

fuse :: [Dur] -> [Dur -> Music a] -> [Music a]
fuse durations notes = 
    map (\(d, n) -> n d) $ zip durations notes




--- EXERCISE 3.11 --- 

-- Define a function !!
--
--     chrom :: Pitch -> Pitch -> Music Pitch
--
-- which constructs an ascending or descending chromatic
-- scale passage from the first note to second.


chrom :: Dur -> Pitch -> Pitch -> Music Pitch
chrom duration p1 p2 =
    let
        a1 = absPitch p1
        a2 = absPitch p2
    in
        line $ map (note duration . pitch) [a1..a2]


-- Test:
-- > playDev 2 $ chrom (1/4) a220 a440




--- EXERCISE 3.12 ---

-- A scale is characterized by intervals.  For example, the
-- major scale is defined by the intervals [2,2,1,2,2,2].
-- Define a function
--
--     mkSkale :: Pitch -> [Int] -> Music Pitch
--
-- such that `mkScale p ints` is the scale beginning at
-- pitch p hand having the interval structure ints.

-- Given an initial pitch and a list of intervals,
-- return a list of absolute pitches beginning
-- with the given pitch and having the given
-- interval structure
--




mkAbsoluteScalePitches :: Pitch -> [Int] -> [Int]
mkAbsoluteScalePitches p ints = 
    reverse $ 
      foldl (\ns k -> head ns + k : ns) [absPitch p]  ints


mkScalePitches :: Pitch -> [Int] -> [Pitch]
mkScalePitches p ints = 
    map pitch $ mkAbsoluteScalePitches p ints


mkScale :: Pitch -> [Int] -> Music Pitch 
mkScale p ints = line $ map (note (1/4)) $ mkScalePitches p ints 

-- Examples:
--
-- > io = mkScale (makePitch C 4) [2,2,1,2,2,2,1]
-- > p io
--
-- > dor = mkScale (makePitch C 4) [2,1,2,2,2,1,2]
-- > p dor



--- EXERCISE 3.13 ---


data Mode_ = Ionian_ | Dorian_ | Phrygian_ | Lydian_ | Mixolydian_ | Aeolian_ | Locrian_

genScale :: Mode_ -> Pitch -> Music Pitch 
genScale mode p =
    case mode of
        Ionian_ -> mkScale p [2,2,1,2,2,2,1]
        Dorian_ -> mkScale p [2,1,2,2,2,1,2]
        Phrygian_ -> mkScale p [1,2,2,2,1,2,2]
        Lydian_ -> mkScale p [2,2,2,1,2,2,1]
        Mixolydian_ -> mkScale p [2,2,1,2,2,1,2]
        Aeolian_ -> mkScale p [2,1,2,2,1,2,2]
        Locrian_ -> mkScale p [1,2,2,1,2,2,2]

-- Test:
-- > playDev 3 $ genScale Phrygian_ (makePitch C 4)
