#!/usr/bin/env python

i = 2520
j = 1
looping = True

while i <= 100:
	squaresum += i
	sumsquare += i ** 2
	i += 1

squaresum = squaresum ** 2
diff = squaresum - sumsquare

print diff
