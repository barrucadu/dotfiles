#!/usr/bin/env python

from numbermagic import *

i = 2
highest = 0

if isprime(600851475143): print "600851475143"
else:
	while i <= sqrt(600851475143):
		if isprime(i) and 600851475143 % i == 0:
			print i
			highest = i
		i += 1
	print i
