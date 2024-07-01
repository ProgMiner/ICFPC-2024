#!/usr/bin/env python3

import sys


def calc_point(x, y, vx, vy, p):
    tx, ty = p

    path = ''
    while x != tx or y != ty:
        dx, dy = tx - x, ty - y
        dvx, dvy = dx - vx, dy - vy

        move = ''
        if dvy < 0 and dvx < 0:
            vx -= 1
            vy -= 1
            move = '1'

        elif dvy < 0 and dvx == 0:
            vy -= 1
            move = '2'

        elif dvy < 0 and dvx > 0:
            vx += 1
            vy -= 1
            move = '3'

        elif dvy == 0 and dvx < 0:
            vx -= 1
            move = '4'

        elif dvy == 0 and dvx == 0:
            move = '5'

        elif dvy == 0 and dvx > 0:
            vx += 1
            move = '6'

        elif dvy > 0 and dvx < 0:
            vx -= 1
            vy += 1
            move = '7'

        elif dvy > 0 and dvx == 0:
            vy += 1
            move = '8'

        elif dvy > 0 and dvx > 0:
            vx += 1
            vy += 1
            move = '9'

        x += vx
        y += vy
        path += move

    return (x, y, vx, vy, path)


points = set()

try:
    while True:
        x, y = [int(x) for x in input().split()]

        points.add((x, y))

except EOFError:
    pass


x, y, vx, vy = 0, 0, 0, 0
res = ''

points_len = len(points)

while points:
    p = min(points, key=lambda p: (p[0] - x)**2 + (p[1] - y)**2)
    points.remove(p)

    x, y, vx, vy, path = calc_point(x, y, vx, vy, p)

    print('Progress:', points_len - len(points), '/', points_len, 'l: ', len(res),
        end='\r', file=sys.stderr)

    res += path

print(res)
