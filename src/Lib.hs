module Lib where

import Euterpea



-- Repeat the music m n times
mrepeat :: Int -> Music a -> Music a
mrepeat 0 m = rest 0
mrepeat n m = m :+: mrepeat (n - 1) m

-- Compute the length of a melody in beats
mlength :: Music a -> Dur
mlength mp =
    case mp of 
        (Prim (Note d p)) -> d
        Prim (Rest d) -> d
        m1 :+: m2 -> mlength m1 + mlength m2
        m1 :=: m2 -> maximum [mlength m1, mlength m2]
        Modify control m -> mlength m


-- Compute the length of a melody in beats, but convert
-- to an integer.  Oops, we loose fractional beats,
-- if there are any.
ilength :: Music Pitch -> Int 
ilength mp = round $ mlength mp

-- Least common multiple of a list of integers.
lcm_ :: [Int] -> Int
lcm_ [] = 1
lcm_ (x:[]) = x
lcm_ (x:xs) = lcm x (lcm_ xs)

--Rescale the durations in a piece by a factor
rescale :: Dur -> Music Pitch -> Music Pitch
rescale factor mp =
    case mp of 
        (Prim (Note d p)) -> Prim (Note (factor * d) p)
        Prim (Rest d) -> Prim (Rest (factor * d))
        m1 :+: m2 -> rescale factor m1 :+: rescale factor m2
        m1 :=: m2 -> rescale factor m1 :=: rescale factor m2
        Modify control m -> Modify control (rescale factor m)





perfTime :: Rational -> Music a -> Rational
perfTime metronome x = dur x * (metronome / 60)


rit r = Modify (Phrase [Tmp $ Ritardando r])
acc a = Modify (Phrase [Tmp $ Accelerando a])
dim d = Modify (Phrase [Dyn $ Diminuendo d])
cre c = Modify (Phrase [Dyn $ Crescendo c])
