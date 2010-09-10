#!/usr/bin/env python

from numbermagic import *

i = 3
total = 2

while i < 2000000:
	if isprime(i): total += i
	i += 2

print total
