#!/usr/bin/env python

from numbermagic import *

primes = 1
i = 3

while primes < 10001:
	if isprime(i):
		primes += 1
	i += 2

print str(i - 2)
