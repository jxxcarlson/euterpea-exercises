
> module RandomMusic where
> import Euterpea
> import System.Random

Generating music with random numbers
Last modified: 22-July-2016
Donya Quick

https://github.com/Euterpea/Euterpea2-Examples/blob/master/NoteLevel/RandomMusic.lhs

Additions: 17-November-2020
James Carlson

This example will illustrate the basics of using random numbers 
to create novel musical structures with Euterpea. First, we'll 
look at how to create infinite sequences of random numbers to use 
as musical "inspiration," and then we'll look at mapping those 
values to things we can hear.

The randInts function below will make an infintie series of random 
Ints from a seed. 

> randInts :: Int -> [Int]
> randInts seed = recInts (mkStdGen seed) where
>     recInts g = let (i,g') = next g in i : recInts g'

This numbers from randInts will be over the entire range of the 
integers, so we will need to take the modulo a base to keep them 
in a more usable range. The function below creates a random 
series of integers within a user-specified range.

> randIntsRange :: (Int, Int) -> Int -> [Int]
> randIntsRange (lower, upper) = 
>     map (\i -> (i `mod` (upper-lower)) + lower) . randInts 

We can use this function to generate random pitches and volumes,
the standard MIDI range for each is 0-127.

The function below will create a random "melody" using a 
specified random number seed, s. Each note will have the 
duration of a sixteenth note (sn). 

> melGen :: Int -> Music (Pitch, Volume)
> melGen s = 
>     let pitches = map pitch $ randIntsRange (30,80) s
>         vols = randIntsRange (40,100) (s+1)
>     in  line $ map (note sn) $ zip pitches vols

Because Euterpea supports infinite playback, we can actually 
listen to one of these melodies indefinitely! Try calling 
the function below from GHCi:

> infinitePlay = play $ melGen 42

Finally we use this function to create three lines in parallel,
each affected by some Control options. To make this piece finite, 
we will use Euterpea's "cut" function to take a fixed number of
measures from the otherwise infinite series that melGen produces.
Various Modify constructors are used to alter the performance of 
each instrument's part: speeding up (Accelerando), slowing down 
(Ritardando), and getting louder (Crescendo). The tempo (Tmp) 
modifiers result in interesting rhythmic textures, such that the 
voices do not play in lock-step.

> somethingWeird = 
>     let part1 = instrument Xylophone $ dim $ rit $ cut 6 $ melGen 345
>         part2 = instrument Marimba $ cut 4 $ melGen 234
>         part3 = instrument TubularBells $ cre $ acc $ cut 8 $ melGen 789
>     in  chord [part1, part2, part3] where
>     rit = Modify (Phrase [Tmp $ Ritardando 0.5])
>     acc = Modify (Phrase [Tmp $ Accelerando 0.5])
>     dim = Modify (Phrase [Dyn $ Diminuendo 0.5])
>     cre = Modify (Phrase [Dyn $ Crescendo 0.5])



ADDITIONS (James Carlson)

  
> randomWalk :: Int -> [Int]
> randomWalk seed = recInts seed (mkStdGen seed) where
>     recInts current g = let 
>                             (i,g') = next g 
>                             delta = 2 * (mod i 2) - 1
>                             current' = current + delta
>                           in current' : recInts current' g'

> boundedRandomWalk :: (Int, Int) -> Int -> Int -> [Int]
> boundedRandomWalk (lowerBound, upperBound) step seed = recInts seed (mkStdGen seed) where
>     recInts current g = let 
>                             (i,g') = next g 
>                             delta = (2 * (mod i 2) - 1) * step
>                             current' = bounce (lowerBound, upperBound) step $ current + delta
>                           in current' : recInts current' g'

> boundedRandomWalk2 :: (Int, Int) -> Int -> Int -> Int -> [Int]
> boundedRandomWalk2 (lowerBound, upperBound) start step seed = recInts (seed + 1000) (mkStdGen seed) where
>     recInts current g = let 
>                             (i,g') = next g 
>                             delta = (2 * (mod i 2) - 1) * step
>                             current' = bounce2 (lowerBound, upperBound) start step $ current + delta
>                           in current' : recInts current' g'


> bounce :: (Int, Int) -> Int -> Int -> Int
> bounce (lowerBound, upperBound) step k = 
>   if k > upperBound then upperBound - step else if k < lowerBound then lowerBound + step else  k

> bounce2 :: (Int, Int) -> Int -> Int -> Int -> Int
> bounce2 (lowerBound, upperBound) start step k = 
>   if k > 1000 then start else if k > upperBound then upperBound - step else if k < lowerBound then lowerBound + step else  k

> melGen1 :: (Int, Int) -> Int -> Music (Pitch, Volume)
> melGen1 (lowerBound, upperBound) s = 
>     let pitches = map pitch $ randIntsRange (lowerBound, upperBound) s
>         vols = randIntsRange (40,100) (s+1)
>     in  line $ map (note sn) $ zip pitches vols

> melGen2 :: (Int, Int) -> Int -> Int -> Int -> Music (Pitch, Volume)
> melGen2 (lowerBound, upperBound) start step seed = 
>     let pitches = map pitch $ boundedRandomWalk2 (lowerBound, upperBound) start step seed
>         vols = randIntsRange (40,100) (seed + 1)
>     in  line $ map (note sn) $ zip pitches vols

> somethingWeird2 = 
>     let part1 = instrument Xylophone $ dim $ rit $ cut 6 $ melGen2 (30, 80) 60 1 345
>         part2 = instrument Marimba $ cut 4 $ melGen2 (30, 80) 50 1 234
>         part3 = instrument TubularBells $ cre $ acc $ cut 8 $ melGen2 (30, 80) 45 1 789
>     in  chord [part1, part2, part3] where
>     rit = Modify (Phrase [Tmp $ Ritardando 0.5])
>     acc = Modify (Phrase [Tmp $ Accelerando 0.5])
>     dim = Modify (Phrase [Dyn $ Diminuendo 0.5])
>     cre = Modify (Phrase [Dyn $ Crescendo 0.5])

> somethingWeird3 = 
>     let part1' = instrument Xylophone $ dim 0.2 $ rit 0.5 $ cut 5 $ melGen1 (30, 80) 345
>         part1 = part1' :+: rest 3 :+: part1'
>         part2 = instrument Marimba $ cre 0.5 $ acc 0.5 $ cut 10 $ melGen2 (30, 80) 30 2 234
>         part3 = instrument TubularBells $ cut 4 $ melGen2 (45, 80) 45 4 789
>     in  chord [part1, part2, part3] where


> rit r = Modify (Phrase [Tmp $ Ritardando r])
> acc a = Modify (Phrase [Tmp $ Accelerando a])
> dim d = Modify (Phrase [Dyn $ Diminuendo d])
> cre c = Modify (Phrase [Dyn $ Crescendo c])

> xylophone :: Dur -> Int -> Int -> Dur -> Dur -> Int -> Music (Pitch, Volume)
> xylophone  n l h d r s = instrument Xylophone $ dim d $ rit r $ cut n $ melGen1 (l, h) (345 + s)

> marimba :: Dur -> Int -> Music (Pitch, Volume)
> marimba n s = instrument Marimba $ cre 0.5 $ acc 0.4  $ cut n $ melGen2 (30, 80) 30 2 (234 + s)
> tubularBells n l h s = instrument TubularBells $ cut n $ melGen2 (l, h) 45 4 (789 + s)


> xylo s = xylophone 5 10 110 0 0 s :+: rest 2 :+: xylophone (2 + hn) 10 110 0.5 0.5 (2 * s)
> bells s = rest 1 :+: tubularBells 2 30 60 s :+: rest 1 :+: tubularBells 2 20 100 (2 * s)

> weird s = chord [xylo s, bells s, marimba 12 s]