module Experiments where

import Euterpea

-- PROCEDURE
--
-- $ stack ghci
-- > import Euterpea
-- > devices -- check to see which device to use 
--
-- Input devices:
--   InputDeviceID 0	IAC Driver Bus 1

-- Output devices:
--   OutputDeviceID 1	IAC Driver Bus 1
--   OutputDeviceID 2	sforzando
--
-- playDev ex1
-- > play 2 line [c 4 qn, c 4 qn, g 4 qn, g 4 qn, a 4 qn, a 4 qn, g 4 hn]


x1 = c 4 en :+: g 4 en :+: c 5 en :+: g 5 en
x2 = x1 :+: transpose 3 x1
x3 = x2 :+: x2 :+: invert x2 :+: retro x2
x4 = forever x3 :=: forever (tempo (2/3) x3)




-- > playDev 3 x4

-- Add a half note rest

x4' = forever x3 :=: (rest hn :+: forever (tempo (2/3) x3))


--- CHAPTER 2 NOTES ---

--  = playDev 2 -- Convenience function

-- My reading of the text is that 
-- note (1/2) (C,4) should produce a Music Pitch
-- value and therefore be playable.  Alas, this 
-- is not the case:
--
-- > :t note (1/2) (C,4)
-- note (1/2) (C,4) :: Num b => Music (PitchClass, b)
--
-- By comparison, we have this, which does produce
-- a playable value:
--
-- > :t note (1/2) (mp C 4)
-- note (1/2) (mp C 4) :: Music Pitch
--
--
-- Go figure!  So we introduce the function mp
-- (make pitch):

-- Maka a pitch 
mp :: PitchClass -> Int ->  Pitch
mp  pitchClass octave =
    (pitchClass, octave)

-- Let's check it out:
-- > import Experiments 
-- > p = playDev 2 -- convenience function (I am lazy)
-- > p $ note (1/2) (mp C 4) 
-- A half note C in octave 4 is played   
