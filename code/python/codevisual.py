#!/usr/bin/env python

import sys, random, time

lines   = []
columns = []
blocks = []

displaymode = "columns"

for argv in sys.argv:
	if argv == "-blocks":
		displaymode = "blocks"
	else:
		file = open(argv)
		for line in file.readlines(1024):
			lines.append(line)
			columns.append([0, random.randint(1, 1000000)])

			block = line.split()
			for blk in block:
				blocks.append(blk)

if displaymode == "columns":
	line = ""
	oldline = ""
	while True:
		line = ""
		col = 0
		
		for column in columns:
			if random.randint(1, 1000000) >= column[1] and len(lines[column[0]]) >= col and len(lines[column[0]]) > 0:
				try:
					line += lines[column[0]]
					column[0] += 1
					if column[0] > len(lines):
						column[0] = 0
				except IndexError:
					line += " "
			elif len(oldline) >= col and len(oldline) > 0:
				try:
					line += oldline[col]
				except IndexError:
					line += " "
			col += 1

		oldline = line
		print line
		time.sleep(random.randint(1, 1000) / 1000.0)
		
elif displaymode == "blocks":
	while True:
		numblocks = random.randint(1, 20)
		line = ""

		for i in range(0, numblocks):
			line += blocks[random.randint(0, len(blocks) - 1)] + " "
		
		print line
		time.sleep(random.randint(1, 1000) / 1000.0)
