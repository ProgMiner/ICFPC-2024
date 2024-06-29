# ICFP Contest 2024

## Files

- `Main.hs` - parser + interpreter of ICFP language
- `print_string.py` - converter from ASCII to ICFP encoding
- `communicate.sh` - script for communication with server
- `token.txt` - token for communication

Y-combinator: `L! B$ L" B$ v! B$ v" v" L" B$ v! B$ v" v"`

## LambdaMan

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
