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