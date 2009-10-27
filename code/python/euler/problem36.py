#!/usr/bin/env python
from numbermagic import *

total = 0

for i in range(1, 1000001):
	if ispalindrome(i):
		if ispalindrome(tobinary(i)):
			total += i

print total
