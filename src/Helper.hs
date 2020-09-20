module Helper where

import Euterpea

-- Make Pitch
-- > makePitch C 4
-- (C,4)
makePitch :: PitchClass -> Int ->  Pitch
makePitch  pitchClass octave =
    (pitchClass, octave)

a220 = makePitch A 3
a440 = makePitch A 4
