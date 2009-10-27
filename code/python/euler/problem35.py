#!/usr/bin/env python
from numbermagic import *
total = 0

for i in range(2, 1000001):
	if isprime(i):
		circ = True
		k = str(i)
		for j in range(0, len(k)):
			k = k[-1:] + k[0:len(k) - 1]
			if not isprime(int(k)): circ = False
		if circ == True:
			total += 1

print total
