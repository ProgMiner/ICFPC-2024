#!/usr/bin/env python3


TABLE = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!"#$%&\'()*+,-./:;<=>?@[\\]^_`|~ \n'

def to_icfp(c):
    try:
        return chr(TABLE.index(c) + 33)
    except IndexError:
        return c

text = input()
print(''.join([to_icfp(c) for c in text]))
