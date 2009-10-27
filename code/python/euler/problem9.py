#!/usr/bin/env python

ans = 0

for a in range(1, 500):
	for b in range(a, 500):
		c = 1000 - a - b
		if a ** 2 + b ** 2 == c ** 2: ans = a * b * c

print ans
