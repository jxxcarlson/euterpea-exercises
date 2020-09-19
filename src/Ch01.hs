module Ch01 where


import Euterpea

--- Exercise 2.1



majorTriad :: Pitch -> Dur -> Music Pitch
majorTriad p d =
   let
       tonic = note d p 
       third = transpose 4 tonic
       fifth = transpose 7 tonic 
   in 
       tonic :=: third :=: fifth

minorTriad :: Pitch -> Dur -> Music Pitch
minorTriad p d =
   let
       tonic = note d p 
       third = transpose 3 tonic
       fifth = transpose 7 tonic 
   in 
       tonic :=: third :=: fifth       

-- twoFiveOne :: Pitch -> Dur -> Music Pitch
-- twoFiveOne p d =
--     let
--         tonic = note d p
--         second = transpose 2 tonic
--         fifth = transpose 7 tonic
--     in 
--         minorTriad second :+: majorTriad fifth :+: majorTriad tonic


--- HELPER ---


-- Maka a pitch 
mp :: PitchClass -> Int ->  Pitch
mp  pitchClass octave =
    (pitchClass, octave)

