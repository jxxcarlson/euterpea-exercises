module Ch01 where


import Euterpea

--- Exercise 2.1



majorTriad :: Music Pitch -> Music Pitch
majorTriad root =
   let
       third = transpose 4 root
       fifth = transpose 7 root 
   in 
       root :=: third :=: fifth

minorTriad :: Music Pitch -> Music Pitch
minorTriad root =
   let
       third = transpose 3 root
       fifth = transpose 7 root 
   in 
       root :=: third :=: fifth       

twoFiveOne :: Music Pitch -> Music Pitch
twoFiveOne root =
    let
        second = transpose 2 root
        fifth = transpose 7 root
    in 
        minorTriad second :+: majorTriad fifth :+: majorTriad root


-- Test:
-- playDev 2 $ twoFiveOne (f 3 hn)


--- HELPER ---


-- Maka a pitch 
-- mp :: PitchClass -> Int ->  Pitch
-- mp  pitchClass octave =
--     (pitchClass, octave)

