import Data.List
import Data.Maybe

roman = ["M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I"]
decimal = [1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1]

getIndex x list = fromJust (elemIndex x list)

getDec r = decimal !! getIndex r roman

toDecList str =
    if null str then
        []
    else if elem (take 2 str) roman then
        getDec (take 2 str) : toDecList (drop 2 str)
    else if elem (take 1 str) roman then
        getDec (take 1 str) : toDecList (drop 1 str)
    else
        toDecList (drop 1 str)

toDecimal x = sum (toDecList x)

getRom d = roman !! getIndex d decimal

repeatRom r i =
    if i == 0 then
        ""
    else
        r ++ repeatRom r (i - 1)

divDec x dec =
    if x == 0 then
        ""
    else if div x (head dec) == 0 then
        divDec x (tail dec)
    else
        repeatRom (getRom (head dec)) (div x (head dec)) ++ divDec (mod x (head dec)) (tail dec)

toRoman x
    | 1 <= x && x <= 3999 = divDec x decimal
    | otherwise = ""
