#!/usr/bin/env sh


code=S$(./print_string.py)

echo Code to send: "$code"
echo "$code" | ./Main
echo

resp=$(curl -d "$code" -H "$(cat ./token.txt)" https://boundvariable.space/communicate)

echo Response:
echo "$resp"
echo "$resp" | ./Main
