#!/usr/bin/env python

from numbermagic import *

ans = getfactorial(100)
anss = str(ans)
total = 0

for i in range(0, len(anss)):
	total += int(anss[i])

print total
