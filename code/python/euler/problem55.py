#!/usr/bin/env python

from numbermagic import *

lychrels = 0

for i in range(1, 10000):
	if islychrel(i, 50): lychrels += 1

print lychrels
