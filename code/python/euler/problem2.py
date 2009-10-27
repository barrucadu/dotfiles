#!/usr/bin/env python
a, b = 1, 2
sumtotal = 0
looping = True

while looping:
	a, b = b, b + a
	if a % 2 == 0 and a < 4000000: sumtotal += a
	if a >= 4000000: looping = False

print str(sumtotal)
