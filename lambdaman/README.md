# ICFP Contest 2024

## Files

- `lambdaman.cpp` - poor solver for TSP
- `lambdaman.py` - encoder for paths
- `lambdaman.sh` - pipeline for saving solutions
- `check.cpp` - checker for solutions
- `lambdaman*.icfp` - solutions source code

## LambdaMan

Decoding combinator for paths:

```
Y (\0 -> \1 -> if v1 == 0 then "" else take 1 (drop (v1 % 5) "_LRUD") ++ v0 (v1 / 5))

Y (\0 -> \1 -> if (v1 == 0) "" ((++) (take 1 (drop (v1 % 5) "_LRUD")) (v0 (v1 / 5))))

B$
    L! B$ L" B$ v! B$ v" v" L" B$ v! B$ v" v"
    L! L" ? B= v" I!
        S
        B.
            BT I" BD B% v" I& SyFLO>
            B$ v! B/ v" I&
```
