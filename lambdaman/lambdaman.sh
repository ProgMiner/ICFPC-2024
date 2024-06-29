#!/usr/bin/env sh

echo "lambdaman$1:"
res=$(./lambdaman | ./lambdaman.py | ../print_int.py)

echo Result: "$res"

cmd=$(echo "solve lambdaman$1 " | ../print_string.py)
echo 'B. S'"$cmd"' B$ B$ L! B$ L" B$ v! B$ v" v" L" B$ v! B$ v" v" L! L" ? B= v" I! S B. BT I" BD B% v" I& SyFLO> B$ v! B/ v" I& I'"$res" > "./lambdaman$1.icfp"
