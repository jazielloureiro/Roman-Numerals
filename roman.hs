import Data.List
import Data.Maybe

roman = ["I", "IV", "V", "IX", "X", "XL", "L", "XC", "C", "CD", "D", "CM", "M"]
decimal = [1, 4, 5, 9, 10, 40, 50, 90, 100, 400, 500, 900, 1000]

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

toRoman x =
    if 1 <= x && x <= 3999 then
        divDec x (reverse decimal)
    else
        ""
