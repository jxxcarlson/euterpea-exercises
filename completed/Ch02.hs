module Ch02 where
    
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




--- Exercise 2.2

-- Recall
-- type Octave = Int
-- type Pitch = (PitchClass, Octave)
-- type PitchClass = Cff | Cf | C ...
-- type Dur = Rational
-- data Primitive = Note Dur Pitch
--                  | Rest Dur

data BluesPitchClassType = Ro | MT | Fo | Fi | MS deriving Show

type BluesPitch = (BluesPitchClassType, Octave)

-- data BluesPrimitive = BlueNote Dur BluesPitch 
--                       | Rest Dur 



ro :: Octave -> Dur -> Music BluesPitch
ro o d = Prim (Note d (Ro, o))

mt :: Octave -> Dur -> Music BluesPitch
mt o d = Prim (Note d (MT, o))

fo :: Octave -> Dur -> Music BluesPitch
fo o d = Prim (Note d (Fo, o))

fi :: Octave -> Dur -> Music BluesPitch
fi o d = Prim (Note d (Fi, o))

ms :: Octave -> Dur -> Music BluesPitch
ms o d = Prim (Note d (MS, o))

bluesScale :: Octave -> Dur -> Music BluesPitch
bluesScale o d = ro o d :+: mt o d :+: fo o d :+: fi o d :+: ms o d :+: ro (o + 1) d

mp :: PitchClass -> Int ->  Pitch
mp  pitchClass octave =
    (pitchClass, octave)



fromBlues :: Music BluesPitch -> Music Pitch
fromBlues blueMusic =
    case blueMusic of
        Prim (Note d (Ro, o)) ->  Prim (Note d (mp C o))
        Prim (Note d (MT, o)) ->  Prim (Note d (mp Ef o))
        Prim (Note d (Fo, o)) ->  Prim (Note d (mp F o))
        Prim (Note d (Fi, o)) ->  Prim (Note d (mp G o))
        Prim (Note d (MS, o)) ->  Prim (Note d (mp Bf o))
        m1 :+: m2 -> (fromBlues m1) :+: (fromBlues m2)
        m1 :=: m2 -> (fromBlues m1) :=: (fromBlues m2)
        Modify control m -> Modify control (fromBlues m)
        --  -> Prim (Note 1 (mp C 1))

-- Test:ues
-- > bluesScale 4 (1/4)
-- > playDev 2 $ fromBlues $  bluesScale 4 (1/4)


--- HELPER ---
