#!/usr/bin/env python3

import sys


sys.set_int_max_str_digits(9999999)

TABLE = "_LRUD"

path = input()

res = 0
while path:
    x = TABLE.index(path[-1])
    path = path[:-1]

    res *= 5
    res += x

print(res)

# while res > 0:
#     path += TABLE[res % 5]
#     res //= 5
# 
# print(path)
