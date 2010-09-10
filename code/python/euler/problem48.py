#!/usr/bin/env python
total = 0

for i in range(1, 1001):
	total += i ** i

totals = str(total)
totals = totals[::-1]
totals = totals[0:10]
totals = totals[::-1]

print totals
