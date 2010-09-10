#!/usr/bin/env python

# Simple CPU emulator. See the spec file to know how this thing works.

from sys import stdin, stdout, stderr

ra = 0
rb = 0
stack = [0, 0, 0, 0, 0, 0]
flags = {'debug': 0}

looping = True
opcodes = []
cur = 0
loop = 0

for line in stdin:
	i = 0
	while i < len(line):
		opcodes.append(line[i:i + 4])
		i += 4

while looping == True and cur < len(opcodes):
	opcode = opcodes[cur]

	if opcode == "0x00":
		looping = False
	
	elif opcode == "0x01":
		cur += 1
		flag = opcodes[cur]
		if flag == "0xFD":
			flags['debug'] = 1 - flags['debug']
		else:
			looping = False
	
	elif opcode == "0x02":
		cur += 1
		register = opcodes[cur]
		if register == "0xFF":
			cur += 1
			ra = opcodes[cur]
		elif register == "0xFE":
			cur += 1
			rb = opcodes[cur]
		else:
			looping = False

	elif opcode == "0x03":
		cur += 1
		register = opcodes[cur]
		if register == "0xFF":
			cur += 1
			value = opcodes[cur]
			if value == "0xFF": ra = ra
			elif value == "0xFE": ra = rb
			elif value == "0xFD": ra = flags['debug']
			else: looping = False
		elif register == "0xFE":
			cur += 1
			value = opcodes[cur]
			if value == "0xFF": rb = ra
			elif value == "0xFE": rb = rb
			elif value == "0xFD": rb = flags['debug']
			else: looping = False
		else:
			looping = False
	
	elif opcode == "0x04":
		cur += 1
		register = opcodes[cur]
		if register == "0xFF":
			cur += 1
			ra = str(hex(int(ra, 16) ^ int(opcodes[cur], 16)))
		elif register == "0xFE":
			cur += 1
			rb = str(hex(int(rb, 16) ^ int(opcodes[cur], 16)))
		else:
			looping = False

	elif opcode == "0x05":
		cur += 1
		register = opcodes[cur]
		if register == "0xFF":
			cur += 1
			value = opcodes[cur]
			if value == "0xFF": ra = str(hex(int(ra, 16) ^ int(ra, 16)))
			elif value == "0xFE": ra = str(hex(int(ra, 16) ^ int(rb, 16)))
			elif value == "0xFD": ra = str(hex(int(ra, 16) ^ int(flags['debug'], 16)))
			else: looping = False
		elif register == "0xFE":
			cur += 1
			value = opcodes[cur]
			if value == "0xFF": rb = str(hex(int(rb, 16) ^ int(ra, 16)))
			elif value == "0xFE": rb = str(hex(int(rb, 16) ^ int(rb, 16)))
			elif value == "0xFD": rb = str(hex(int(rb, 16) ^ int(flags['debug'], 16)))
			else: looping = False
		else:
			looping = False
	
	elif opcode == "0x06":
		cur += 1
		register = opcodes[cur]
		if register == "0xFF":
			cur += 1
			ra = str(hex(int(ra, 16) & int(opcodes[cur], 16)))
		elif register == "0xFE":
			cur += 1
			rb = str(hex(int(rb, 16) & int(opcodes[cur], 16)))
		else:
			looping = False

	elif opcode == "0x07":
		cur += 1
		register = opcodes[cur]
		if register == "0xFF":
			cur += 1
			value = opcodes[cur]
			if value == "0xFF": ra = str(hex(int(ra, 16) & int(ra, 16)))
			elif value == "0xFE": ra = str(hex(int(ra, 16) & int(rb, 16)))
			elif value == "0xFD": ra = str(hex(int(ra, 16) & int(flags['debug'], 16)))
			else: looping = False
		elif register == "0xFE":
			cur += 1
			value = opcodes[cur]
			if value == "0xFF": rb = str(hex(int(rb, 16) & int(ra, 16)))
			elif value == "0xFE": rb = str(hex(int(rb, 16) & int(rb, 16)))
			elif value == "0xFD": rb = str(hex(int(rb, 16) & int(flags['debug'], 16)))
			else: looping = False
		else:
			looping = False
	
	elif opcode == "0x08":
		cur += 1
		register = opcodes[cur]
		if register == "0xFF":
			cur += 1
			ra = str(hex(int(ra, 16) | int(opcodes[cur], 16)))
		elif register == "0xFE":
			cur += 1
			rb = str(hex(int(rb, 16) | int(opcodes[cur], 16)))
		else:
			looping = False

	elif opcode == "0x09":
		cur += 1
		register = opcodes[cur]
		if register == "0xFF":
			cur += 1
			value = opcodes[cur]
			if value == "0xFF": ra = str(hex(int(ra, 16) | int(ra, 16)))
			elif value == "0xFE": ra = str(hex(int(ra, 16) | int(rb, 16)))
			elif value == "0xFD": ra = str(hex(int(ra, 16) | int(flags['debug'], 16)))
			else: looping = False
		elif register == "0xFE":
			cur += 1
			value = opcodes[cur]
			if value == "0xFF": rb = str(hex(int(rb, 16) | int(ra, 16)))
			elif value == "0xFE": rb = str(hex(int(rb, 16) | int(rb, 16)))
			elif value == "0xFD": rb = str(hex(int(rb, 16) | int(flags['debug'], 16)))
			else: looping = False
		else:
			looping = False
	
	elif opcode == "0x0A":
		cur += 1
		register = opcodes[cur]
		if register == "0xFF":
			cur += 1
			ra = str(hex(int(ra, 16) + int(opcodes[cur], 16)))
		elif register == "0xFE":
			cur += 1
			rb = str(hex(int(rb, 16) + int(opcodes[cur], 16)))
		else:
			looping = False

	elif opcode == "0x0B":
		cur += 1
		register = opcodes[cur]
		if register == "0xFF":
			cur += 1
			value = opcodes[cur]
			if value == "0xFF": ra = str(hex(int(ra, 16) + int(ra, 16)))
			elif value == "0xFE": ra = str(hex(int(ra, 16) + int(rb, 16)))
			elif value == "0xFD": ra = str(hex(int(ra, 16) + int(flags['debug'], 16)))
			else: looping = False
		elif register == "0xFE":
			cur += 1
			value = opcodes[cur]
			if value == "0xFF": rb = str(hex(int(rb, 16) + int(ra, 16)))
			elif value == "0xFE": rb = str(hex(int(rb, 16) + int(rb, 16)))
			elif value == "0xFD": rb = str(hex(int(rb, 16) + int(flags['debug'], 16)))
			else: looping = False
		else:
			looping = False
	
	elif opcode == "0x0C":
		cur += 1
		register = opcodes[cur]
		if register == "0xFF":
			cur += 1
			ra = str(hex(int(ra, 16) - int(opcodes[cur], 16)))
		elif register == "0xFE":
			cur += 1
			rb = str(hex(int(rb, 16) - int(opcodes[cur], 16)))
		else:
			looping = False

	elif opcode == "0x0D":
		cur += 1
		register = opcodes[cur]
		if register == "0xFF":
			cur += 1
			value = opcodes[cur]
			if value == "0xFF": ra = str(hex(int(ra, 16) - int(ra, 16)))
			elif value == "0xFE": ra = str(hex(int(ra, 16) - int(rb, 16)))
			elif value == "0xFD": ra = str(hex(int(ra, 16) - int(flags['debug'], 16)))
			else: looping = False
		elif register == "0xFE":
			cur += 1
			value = opcodes[cur]
			if value == "0xFF": rb = str(hex(int(rb, 16) - int(ra, 16)))
			elif value == "0xFE": rb = str(hex(int(rb, 16) - int(rb, 16)))
			elif value == "0xFD": rb = str(hex(int(rb, 16) - int(flags['debug'], 16)))
			else: looping = False
		else:
			looping = False

	elif opcode == "0x0E":
		loop = cur

	elif opcode == "0x0F":
		cur = loop

	elif opcode == "0x10":
		cur += 1
		register = opcodes[cur]
		if register == "0xFF":
			stack[0] = stack[1]
			stack[1] = stack[2]
			stack[2] = stack[3]
			stack[3] = stack[4]
			stack[4] = stack[5]
			stack[5] = ra
		elif register == "0xFE":
			stack[0] = stack[1]
			stack[1] = stack[2]
			stack[2] = stack[3]
			stack[3] = stack[4]
			stack[4] = stack[5]
			stack[5] = rb
		else:
			looping = False

	elif opcode == "0x11":
		cur += 1
		register = opcodes[cur]
		if register == "0xFF":
			ra = stack[5]
			stack[5] = stack[4]
			stack[4] = stack[3]
			stack[3] = stack[2]
			stack[2] = stack[1]
			stack[1] = stack[0]
		elif register == "0xFE":
			rb = stack[5]
			stack[5] = stack[4]
			stack[4] = stack[3]
			stack[3] = stack[2]
			stack[2] = stack[1]
			stack[1] = stack[0]
		else:
			looping = False

	elif opcode == "0x12":
		cur += 1
		register = opcodes[cur]
		cur += 1
		register2 = opcodes[cur]
		if register == "0xFF":
			if register2 == "0xFF" and int(ra,16) > int(ra,16): cur += 1
			if register2 == "0xFE" and int(ra,16) > int(rb,16): cur += 1
		if register == "0xFE":
			if register2 == "0xFF" and int(rb,16) > int(ra,16): cur += 1
			if register2 == "0xFE" and int(rb,16) > int(rb,16): cur += 1

	elif opcode == "0x13":
		cur += 1
		register = opcodes[cur]
		cur += 1
		register2 = opcodes[cur]
		if register == "0xFF":
			if register2 == "0xFF" and int(ra,16) < int(ra,16): cur += 1
			if register2 == "0xFE" and int(ra,16) < int(rb,16): cur += 1
		if register == "0xFE":
			if register2 == "0xFF" and int(rb,16) < int(ra,16): cur += 1
			if register2 == "0xFE" and int(rb,16) < int(rb,16): cur += 1

	elif opcode == "0x14":
		cur += 1
		register = opcodes[cur]
		cur += 1
		register2 = opcodes[cur]
		if register == "0xFF":
			if register2 == "0xFF" and int(ra,16) == int(ra,16): cur += 1
			if register2 == "0xFE" and int(ra,16) == int(rb,16): cur += 1
		if register == "0xFE":
			if register2 == "0xFF" and int(rb,16) == int(ra,16): cur += 1
			if register2 == "0xFE" and int(rb,16) == int(rb,16): cur += 1

	cur += 1

if flags['debug'] == 1:
	print ra, rb
	print stack
	print flags
	print opcodes
	print cur
