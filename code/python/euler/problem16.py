#!/usr/bin/env python

num = 2 ** 1000
nums = str(num)
total = 0

for i in range(0, len(nums)):
	total += int(nums[i])

print total
