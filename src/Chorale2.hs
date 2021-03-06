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


module Chorale2 where

import Euterpea


-- The composition:

-- Infinite version: 
-- chorale :: InstrumentName -> Music Pitch
-- chorale player =  instrument player $ bass :=: tenor :=: tenor2 :=: solo


-- -- Finite version of the chorale with instrument = clarinet
-- chorale' :: Music Pitch
-- chorale' = bassLine' :=: tenorLine' :=: tenorLine2' :=: soloLine'


-- Examples:
-- > playDev 2 $ chorale Bassoon
-- > playDev 2 chorale'


x1 = Modify (Phrase [Dyn (Diminuendo (3/4))]) cms

x2 = Modify (Phrase [  Dyn (Crescendo 4), Dyn (Loudness 25)  ]) cms


cresc :: Rational -> Rational ->  Music Pitch -> Music Pitch
cresc c l m = Modify (Phrase [  Dyn (Crescendo c), Dyn (Loudness l)  ]) m

cresc' :: Rational ->  Music Pitch -> Music Pitch
cresc' p m = phrase [ Dyn (Crescendo p) ] m
-- cresc' p m = Modify (Phrase [  Dyn (Crescendo p) ]) m

dim ::  Rational -> Rational ->  Music Pitch -> Music Pitch
dim d l m = Modify (Phrase [  Dyn (Diminuendo d), Dyn (Loudness l)  ]) m

dim' ::  Rational ->  Music Pitch -> Music Pitch
dim' p m = Modify (Phrase [  Dyn (Diminuendo p)  ]) m


cMajScale = Modify (Tempo 2)
            (line [c 4 en, d 4 en, e 4 en, f 4 en, 
                    g 4 en, a 4 en, b 4 en, c 5 en])

cms' = line [c 4 en, d 4 en, e 4 en, f 4 en, 
             g 4 en, a 4 en, b 4 en, c 5 en]

cms = cMajScale

mu2 = Modify (Instrument Vibraphone)
      (Modify (Phrase [Dyn (Diminuendo (3/4))]) cms :+:
         (Modify (Phrase [Dyn (Crescendo 4), Dyn (Loudness 25)]) cms))


bass' = phrase [Dyn (Loudness 60)] $ bass 0 0 Bassoon :=: bass 12 0 FrenchHorn :=: bass 19 4 Clarinet :=: bass 24 6 Oboe

chorale = (mrepeat 3 bass') :=: solo' :=: (mrepeat 10 $ tenorLine 50 FrenchHorn) :=: (mrepeat 9 tenorLine2a)

tenorLine2a = tenorLine2 40 Bassoon 0 :=: tenorLine2 40 Oboe 0 

--- Construction of the chorale ---

solo' = (phrase [Dyn (Loudness 70)] $ solo 7 0 Flute)  :+: (phrase [Dyn (Loudness 70)] $ solo 0 0 Clarinet)

solo ::Int -> Rational  -> InstrumentName -> Music Pitch
solo shift delay player = instrument player $ (rest delay) :+: (transpose shift soloLine)

tenor :: Music Pitch
tenor = forever $ tenorLine 50 FrenchHorn

tenor2 :: Music Pitch
tenor2 = forever $ tenorLine2 40 Bassoon 0 :=: tenorLine2 40 Oboe 0 

bass :: Int -> Rational  -> InstrumentName -> Music Pitch
bass shift delay player = instrument player $ (rest delay) :+: (transpose shift bassLine)

-- Compute the length of each line
lSolo = ilength soloLine
lTenor = ilength $ tenorLine 50 FrenchHorn
lTenor2 = ilength $ tenorLine2 40 Bassoon 0 :=: tenorLine2 40 Oboe 0 
lBass = ilength bassLine

-- Compute the cycle length as the
-- least common multiple of the lengths
-- of the individual lines.
cycleLength = lcm_ [lSolo, lTenor, lTenor2, lBass]

-- Compute the number of times each line of
-- music must be repeated so as to have
-- the given cycle length.
nSolo = cycleLength `div` lSolo
nTenor = cycleLength `div` lTenor
nTenor2 = cycleLength `div` lTenor2
nBass = cycleLength `div` lBass

-- Repeat the music m n times
mrepeat :: Int -> Music a -> Music a
mrepeat 0 m = rest 0
mrepeat n m = m :+: mrepeat (n - 1) m

-- Construct one cycle of the chorale
soloLine', tenorLine', tenorLine2', bassLine' :: Music Pitch
soloLine' = mrepeat nSolo soloLine 
tenorLine' = mrepeat nTenor $ tenorLine 50 FrenchHorn
tenorLine2' = mrepeat nTenor2 $ tenorLine2 40 Bassoon 0
bassLine' = mrepeat nBass bassLine

-- Verify:
-- > map ilength [bassLine', tenorLine', tenorLine2', soloLine']
--   [156780,156780,156780,156780]


-- The bass line
bassNote = d 1 
bassNote' = ds 1
bassNote'' = e 1
bassNote''' = f 1
bassLine =   line $ [ cresc' 0.5 $ bassNote 6, rest 3, bassNote' 3, rest 3, bassNote'' 3 
                    , rest 2, bassNote''' 4, rest 1 ,   dim' 0.5 $ bassNote 7, rest 4]



-- The tenor
tenorNote = a 1
tenorNote' = c 2
tenorNote'' = d 2
tenorLine d i = phrase [ Dyn (Loudness d)] $ instrument i $ line $ map (rescale (1/2)) [rest 2, tenorNote 8, rest 3
                  , tenorNote' 4, tenorNote'' 5, rest 4]

-- The second tenor
tenorLine2 d i t = phrase [ Dyn (Loudness d)] $ instrument i $ transpose t $ rest 7 :+: f 1 4 :+: c 2 4

-- The solo
soloMotifA = line $ [a 2 1, d 3 1, c 4 2, f 4 hn, rest 2]
soloMotif = line $ [a 2 1, d 3 1, c 3 2, f 3 2, e 3 4, rest 2]
soloMotif' = rest 9 :+: soloMotif :+: transpose 3 soloMotif
soloLine = soloMotif' :+: transpose 7 soloMotif' :+: rest 1


-- Compute the length of a melody in beats
mlength :: Music Pitch -> Dur
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


-- not used right now:
rescale :: Dur -> Music Pitch -> Music Pitch
rescale factor mp =
    case mp of 
        (Prim (Note d p)) -> Prim (Note (factor * d) p)
        Prim (Rest d) -> Prim (Rest (factor * d))
        m1 :+: m2 -> rescale factor m1 :+: rescale factor m2
        m1 :=: m2 -> rescale factor m1 :=: rescale factor m2
        Modify control m -> Modify control (rescale factor m)



