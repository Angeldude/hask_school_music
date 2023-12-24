--Exercise 2.1 The above example is fairly concrete, in that, for one, it is rooted in C major, and furthermore it has a fixed tempo. Define a function twoFiveOne :: Pitch → Dur → Music Pitch such that twoFiveOne p d constructs a ii-V-I chord progression in the key whose major scale begins on the pitch p (i.e., the first degree of the major scale on which the progression is being constructed), where the duration of each of the first two chords is d, and the duration of the last chord is 2 * d. To verify your code, prove by calculation that twoFiveOne (C, 4) wn = t251.
import Euterpea

t251 :: Music Pitch
t251 = let dMinor = d 4 wn :=: f 4 wn :=: a 4 wn
           gMajor = g 4 wn :=: b 4 wn :=: d 5 wn
           cMajor = c 4 bn :=: e 4 bn :=: g 4 bn
       in dMinor :+: gMajor :+: cMajor

twoFiveOne :: Pitch -> Dur -> Music Pitch
twoFiveOne p d = 