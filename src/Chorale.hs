module Chorale where

import Euterpea


-- The composition:
chorale :: Music Pitch
chorale =  instrument player bass :=: instrument player tenor 
           :=: instrument player tenor2 :=: instrument player solo


--- Its construction ---

player :: InstrumentName
player = Bassoon

bassNote = d 1 
bassNote' = ds 1
bassNote'' = e 1
bassNote''' = f 1

tenorNote = a 1
tenorNote' = c 2
tenorNote'' = d 2

bigRest :: Music Pitch
bigRest = rest 9

soloMotifA = line $ [a 2 1, d 3 1, c 4 2, f 4 hn, rest 2]
soloMotif = line $ [a 2 1, d 3 1, c 3 2, f 3 2, e 3 4, rest 2]
soloMotif' = rest 9 :+: soloMotif :+: transpose 3 soloMotif
soloMotif'' = soloMotif' :+: transpose 7 soloMotif' :+: rest 1

solo :: Music Pitch
solo = forever $ soloMotif''

tenor :: Music Pitch
tenor = forever $ line $ map (rescale (1/2)) [rest 2, tenorNote 8, rest 3, tenorNote' 4, tenorNote'' 5, rest 4]

tenor2 :: Music Pitch
tenor2 = forever $ rest 7 :+: f 1 4 :+: c 2 4

bass :: Music Pitch
bass = forever $ line $ [bassNote 6, rest 3, bassNote' 3, rest 3, bassNote'' 3 , rest 2, bassNote''' 4, rest 1 , bassNote 7, rest 4]






-- not uxed right now:
rescale :: Dur -> Music Pitch -> Music Pitch
rescale factor mp =
    case mp of 
        (Prim (Note d p)) -> Prim (Note (factor * d) p)
        Prim (Rest d) -> Prim (Rest (factor * d))
        m1 :+: m2 -> rescale factor m1 :+: rescale factor m2
        m1 :=: m2 -> rescale factor m1 :=: rescale factor m2
        Modify control m -> Modify control (rescale factor m)
