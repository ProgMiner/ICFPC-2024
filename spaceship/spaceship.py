#!/usr/bin/env python3


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


def metric(val):
    x, y, vx, vy, path = val

    return (len(path), abs(vx) + abs(vy))


points = set()

try:
    while True:
        x, y = [int(x) for x in input().split()]

        points.add((x, y))

except EOFError:
    pass


x, y, vx, vy = 0, 0, 0, 0
res = ''

while points:
    x, y, vx, vy, path = min([calc_point(x, y, vx, vy, p) for p in points], key=metric)
    points.remove((x, y))

    res += path

print(res)
