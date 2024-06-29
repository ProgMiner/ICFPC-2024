#!/usr/bin/env sh


if [ "$1" = '-r' ] ; then
    read -r code
else
    code=S$(./print_string.py)
fi

echo Code to send: "$code"
# echo "$code" | ./Main
echo

resp=$(curl -d "$code" -H "$(cat ./token.txt)" https://boundvariable.space/communicate)

echo Response:
echo "$resp"
echo "$resp" | ./Main
