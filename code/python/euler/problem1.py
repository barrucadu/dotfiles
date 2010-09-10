#!/usr/bin/env python
i = 3
sumtotal = 0

while i < 1000:
	if i % 3 == 0: sumtotal += i
	elif i % 5 == 0 and not i % 3 == 0: sumtotal += i
	i += 1

print str(sumtotal)
