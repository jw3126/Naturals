# Naturals

Peano style definition of natural numbers. These do not satsfy the induction principle however, as you can define "infinite" numbers.
```Haskell
*Main Lib> inf = suc inf
*Main Lib> inf > zero
True
```
Our Naturals are codata instead of data.
