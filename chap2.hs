--Exercise 2.1 The above example is fairly concrete, in that, for one, it is rooted in C major, and furthermore it has a fixed tempo. 
-- Define a function twoFiveOne :: Pitch → Dur → Music Pitch such that twoFiveOne p d constructs a ii-V-I 
-- chord progression in the key whose major scale begins on the pitch p (i.e., the first degree of the major scale on which the progression is being constructed), 
-- where the duration of each of the first two chords is d, and the duration of the last chord is 2 * d. To verify your code, prove by calculation that twoFiveOne (C, 4) wn = t251.
import Euterpea

t251 :: Music Pitch
t251 = let dMinor = d 4 wn :=: f 4 wn :=: a 4 wn
           gMajor = g 4 wn :=: b 4 wn :=: d 5 wn
           cMajor = c 4 bn :=: e 4 bn :=: g 4 bn
       in dMinor :+: gMajor :+: cMajor

twoFiveOne :: Pitch -> Dur -> Music Pitch
twoFiveOne p d = let 
                    two = note d (trans 2 p) :=: note d (trans 5 p) :=: note d (trans 9 p)
                    five = note d (trans 7 p) :=: note d (trans 11 p) :=: note d (trans 14 p)
                    one =  note (d * 2) p :=: note (d * 2) (trans 4 p) :=: note (d * 2) (trans 7 p)
                 in two :+: five :+: one
                 
-- Exercise 2.2 The PitchClass data type implies the use of standard Western
-- harmony, in particular the use of a 12-tone equal temperament scale. But there
-- are many other scale possibilities. For example, the pentatonic blues scale
-- consists of five notes (thus “pentatonic”), and in the key of C approximately
-- corresponds to the notes C, Eb, F, G, and Bb. More abstractly, let’s call these
-- the root, minor third, fourth, fifth, and minor seventh, respectively. Your job
-- is to:
-- 1. Define a new algebraic data type called BluesPitchClass that captures this
-- scale (for example, you may wish to use the constructor names Ro, MT,
-- Fo, Fi, and MS).
-- 2. Define a type synonym BluesPitch, akin to Pitch.
-- 3. Define the auxiliary functions ro, mt, fo, fi, and ms, akin to those in
-- Figure 2.2, that make it easy to construct notes of type Music BluesPitch.
-- 4. In order to play a value of type Music BluesPitch using MIDI, it will have
-- to be converted into a Music Pitch value. Define a function
-- fromBlues :: Music BluesPitch → Music Pitch to do this, using the
-- “approximate” translation described at the beginning of this exercise.
-- Hint: To do this properly, you will have to pattern match against the Music
-- value, something like this:
-- fromBlues (Prim (Note d p)) = ...
-- fromBlues (Prim (Rest d)) = ...
-- fromBlues (m1 :+: m2) = ...
-- fromBlues (m1 :=: m2) = ...
-- fromBlues (Modify...) = ...
-- 5. Write out a few melodies of type Music BluesPitch, and play them using
-- fromBlues and play

data BluesPitchClass = Ro | MT | Fo | Fi | MS
type BluesPitch = (BluesPitchClass, Octave)

ro, mt, fo, fi, ms :: Octave -> Dur -> Music BluesPitch
ro o d = note d (Ro, o)
mt o d = note d (MT, o)
fo o d = note d (Fo, o) 
fi o d = note d (Fi, o)
ms o d = note d (MS, o)

fromBlues :: Music BluesPitch -> Music Pitch
fromBlues (Prim (Note d (Ro, o))) = note d (C, o)
fromBlues (Prim (Note d (MT, o))) = note d (Ef, o)
fromBlues (Prim (Note d (Fo, o))) = note d (F, o)
fromBlues (Prim (Note d (Fi, o))) = note d (G, o)
fromBlues (Prim (Note d (MS, o))) = note d (Bf, o)
fromBlues (Prim (Rest d)) = rest d
fromBlues (m1 :+: m2) = fromBlues m1 :+: fromBlues m2
fromBlues (m1 :=: m2) = fromBlues m1 :=: fromBlues m2
fromBlues (Modify cntrl m) = Modify cntrl (fromBlues m)

melody oct = ro oct qn :+: mt oct qn :+: ms oct en :+: fo oct en :=: ms oct en :+: fi oct qn :=: ms oct qn :+: fo oct en :=: ms oct en

melody2 oct = ro oct en :+: mt oct qn :+: ro oct en :+: mt oct qn :+: ro oct hn :+: rest hn :+: fi oct en :+: fo oct qn :+: mt oct en :+: ro (oct+1) hn :=: fo oct wn :=: ms oct wn

-- Exercise 2.3 Show that abspitch (pitch ap) = ap and, up to enharmonic equivalences, pitch (abspitch p) = p.

abspitchEquiv = absPitch (pitch 60) == 60
-- absPitch (C, 4) == 60
-- 60 == 60
pitchEquiv = pitch (absPitch (Bs, 3)) == (C, 4)
-- pitch (60) == (C, 4)
-- (C,4) == (C, 4)

-- Exercise 2.4 Show that trans i (trans j p) = trans (i + j) p.
transposed = trans 4 (trans 5 (C, 4)) == trans 9 (C, 4)
-- trans 4 (trans 5 (C, 4)) == (A, 4)
-- trans 4 (F, 4) == (A, 4)
-- (A, 4) == (A, 4)

-- Exercise 2.5 Transpose is part of the Control data type, which in turn is part of the Music data type. Its use in transposing a Music value is thus a kind of “annotation”—it doesn’t really change the Music value, it just annotates it as something that is transposed. Define instead a recursive function transM ::AbsPitch → Music Pitch → Music Pitch that actually changes each note in a Music Pitch value by transposing it by the interval represented by the first argument.
-- Hint: To do this properly, you will have to pattern match against the Music value, something like this:
-- transM ap (Prim (Note d p)) = ...
-- transM ap (Prim (Rest d)) = ...
-- transM ap (m1 :+: m2) = ...
-- transM ap (m1 :=: m2) = ...
-- transM ap (Modify...) = ...

transM :: AbsPitch -> Music Pitch -> Music Pitch
transM ap (Prim (Rest d)) = rest d
transM ap (Prim (Note d p)) = note d (trans ap p)
transM ap (m1 :+: m2) = transM ap m1 :+: transM ap m2
transM ap (m1 :=: m2) = transM ap m1 :=: transM ap m2
transM ap (Modify cntrl m) = Modify cntrl (transM ap m)