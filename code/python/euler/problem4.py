#!/usr/bin/env python

from numbermagic import *

palindrome = 0

for i in range(100, 999):
	for j in range(100, 999):
		x = i * j
		if ispalindrome(x) and x > palindrome: palindrome = x

print palindrome
