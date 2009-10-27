#!/usr/bin/env python

bigcount = 0
n = 0

for i in range(1, 1000000):
	j = i
	curcount = 0
	while not j == 1:
		if j % 2 == 0:
			j = j / 2
		else:
			j = 3 * j + 1
		curcount += 1
	if curcount > bigcount:
		bigcount = curcount
		n = i

print n
