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
     
-- Exercise 3.2 Show that flip (flip f) is the same as f .
-- flip (flip f x y)
-- flip f y x
-- f x y
-- Exercise 3.3 Define the type of ys in:
-- xs = [1, 2, 3] :: [Integer]
-- ys = map (+) xs
-- ys :: [Integer -> Integer]
     
-- Exercise 3.4 Define a function applyEach that, given a list of functions,
-- applies each to some given value. For example:
-- applyEach [simple 2 2,(+3)] 5 => [14, 8]
-- where simple is as defined in Chapter 1

applyEach :: [(a -> b)] -> a -> [b]
applyEach [] _ = []
applyEach (fn:fns) val = fn val : applyEach fns val

applyEach' fns val = let flipped x fn = fn x
                    in map (flipped val) fns

applyEach'' fns val = map (flip id val) fns

applyEach''' fns val = map ($ val) fns

-- Exercise 3.5 Define a function applyAll that, given a list of functions
-- [f1, f2, ..., fn] and a value v, returns the result f1 (f2 (...(fn v)...)). For
-- example:
-- applyAll [simple 2 2,(+3)] 5 => 20

simple x y z = x * (y + z)

applyAll :: [(a -> a)] -> a -> a
applyAll fns v = let f op acc = op acc
                 in foldr f v fns

applyAll' :: [(a -> a)] -> a -> a                 
applyAll' fns v = foldr ($) v fns

-- Exercise 3.7 Rewrite the definition of length non-recursively
length' :: [a] -> Integer
length' xs = let plusOne _ y = y + 1
             in foldr plusOne 0 xs
             
-- Exercise 3.8 Define a function that behaves as each of the following:
-- a) Doubles each number in a list. For example:
-- doubleEach [1, 2, 3] => [2, 4, 6]
-- b) Pairs each element in a list with that number and one plus that number. For
-- example:
-- pairAndOne [1, 2, 3] => [(1, 2),(2, 3),(3, 4)]
-- c) Adds together each pair of numbers in a list. For example:
-- addEachPair [(1, 2),(3, 4),(5, 6)] => [3, 7, 11 ]
-- d) Adds “pointwise” the elements of a list of pairs. For example:
-- addPairsPointwise [(1, 2),(3, 4),(5, 6)] => (9, 12)

doubleEach :: [Integer] -> [Integer]
doubleEach = map (* 2)

pairAndOne :: [Integer] -> [(Integer, Integer)]
pairAndOne = let pairing x = (x, x+1)
             in map pairing
 
addEachPair :: [(Integer, Integer)] -> [Integer] 
addEachPair = let addPair (x,y) = x + y
              in map addPair

addPairsPointwise :: [(Integer, Integer)] -> (Integer, Integer)              
addPairsPointwise xs = let pointWise (a,b) (c,d) = (a+c, b+d)
                       in foldr1 pointWise xs

doesItAll = addPairsPointwise . pairAndOne . addEachPair . pairAndOne . doubleEach