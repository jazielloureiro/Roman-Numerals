module Roman (toDecimal, toRoman) where

import Data.List
import Data.Maybe

roman = ["M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"]
decimal = [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1]

getIndex x list = fromJust (elemIndex x list)

getDec r = decimal !! getIndex r roman

toDecList "" = []
toDecList rom
    | elem fstTwo roman = getDec fstTwo : toDecList dropTwo
    | elem fstOne roman = getDec fstOne : toDecList dropOne
    | otherwise = toDecList dropOne
    where fstTwo = take 2 rom
          fstOne = take 1 rom
          dropTwo = drop 2 rom
          dropOne = drop 1 rom

toDecimal x = sum (toDecList x)

getRom d = roman !! getIndex d decimal

repeatRom r 0 = ""
repeatRom r i = r ++ repeatRom r (i - 1)

divDec 0 _ = ""
divDec x (d:ds)
    | q == 0 = divDec x ds
    | otherwise = repeatRom (getRom d) q ++ divDec (mod x d) ds
    where q = div x d

toRoman x
    | 1 <= x && x <= 3999 = divDec x decimal
    | otherwise = ""
