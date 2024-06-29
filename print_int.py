#!/usr/bin/env python3

import sys


sys.set_int_max_str_digits(999999)

n = int(input())

if n < 0:
    raise ValueError('negative number')

if n == 0:
    print('!')

else:
    res = ''

    while n > 0:
        x = n % 94
        n = n // 94

        res = chr(x + ord('!')) + res

    print(res)
