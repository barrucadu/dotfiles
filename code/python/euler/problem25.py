#!/usr/bin/env python
a, b = 1, 1
i = 2

while len(str(b)) < 1000:
	a, b = b, b + a
	i += 1

print i
