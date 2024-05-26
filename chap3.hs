import Euterpea

-- Exercise 3.1 Using map, define:

-- 1.A function f1 :: Int → [Pitch] → [Pitch] that transposes each pitch
-- in its second argument by the amount specified in its first argument.
-- 2.A function f2 :: [Dur] → [Music a] that turns a list of durations 
-- into a list of rests, each having the corresponding duration.
-- 3.A function f3 :: [Music Pitch] → [Music Pitch] that takes a list of music values (that are assumed to be single notes) and, for each such
-- note, halves its duration and places a rest of that same duration after it. For example:
-- f3 [c 4 qn, d 4 en, e 4 hn] => [c 4 en :+: rest en, d 4 sn :+: rest sn,
 -- e 4 qn :+: rest qn]
-- You can think of this as giving a staccato interpretation of the notes.

f1 :: Int -> [Pitch] -> [Pitch]
f1 t ps = map (trans t) ps

f2 :: [Dur] -> [Music a]
f2 = map rest

f3 :: [Music Pitch] -> [Music Pitch]
f3 ps = let f (Prim (Note d p)) = (note (d/2) p) :+: rest (d/2)
        in map f ps