from math import sqrt

def tobinary(n):
	bin = ""

	if n == 0: return 0
	else:
	
		while n > 0:
			bin = str(n % 2) + bin
			n = n / 2
		return bin

def toword(n):
	if n == 0: return "zero"
	elif n == 1: return "one"
	elif n == 2: return "two"
	elif n == 3: return "three"
	elif n == 4: return "four"
	elif n == 5: return "five"
	elif n == 6: return "six"
	elif n == 7: return "seven"
	elif n == 8: return "eight"
	elif n == 9: return "nine"
	elif n == 10: return "ten"
	elif n == 11: return "eleven"
	elif n == 12: return "twelve"
	elif n == 13: return "thirteen"
	elif n == 14: return "fourteen"
	elif n == 15: return "fifteen"
	elif n == 16: return "sixteen"
	elif n == 17: return "seventeen"
	elif n == 18: return "eighteen"
	elif n == 19: return "nineteen"
	elif n == 20: return "twenty"
	elif n == 30: return "thirty"
	elif n == 40: return "forty"
	elif n == 50: return "fifty"
	elif n == 60: return "sixty"
	elif n == 70: return "seventy"
	elif n == 80: return "eighty"
	elif n == 90: return "ninety"
	elif n == 100: return "hundred"
	elif n == 1000: return "thousand"
	elif n == 1000000: return "million"
	else: return False

def getdivisors(num, includeone, includeend):
	divisors = []
	i = 1
	while i <= num:
		if num % i == 0:
			if i == 1 and includeone == True: divisors.append(i)
			elif i == num and includeend == True: divisors.append(i)
			else: divisors.append(i)
		i += 1
	
	return divisors

def getfactorial(num):
	total = 1
	
	for i in range(1, num):
		total = total * i
	
	return total

def isprime(num):
	divisor = 3
	nums = str(num)
	
	if nums[-1:] == "0" or  nums[-1:] == "2" or nums[-1:] == "4" or nums[-1:] == "5" or nums[-1:] == "6" or nums[-1:] == "8":
		if num == 2 or num == 5: return True
		else: return False
	else:
		while divisor <= sqrt(num):
			if num % divisor == 0: return False
			divisor += 2
		return True

def getprimefactors(num):
	primefactors = []
	i = 2
	numw = num

	if isprime(num) == True: return [num]
	else:
		while i < num:
			if isprime(i):
				if numw % i == 0:
					primefactors.append(i)
					numw = numw / i
				elif i == 2: i = 3
				else: i += 2
			else:
				i += 2
		return primefactors

def getlcm(num):
	primefactors = {}
	multiply = {}
	lcm = 1

	for i in range(0, len(num)):
		primefactors[str(num[i])] = getprimefactors(num[i])

		for j in range(0, len(primefactors[str(num[i])])):
			if multiply.has_key(primefactors[str(num[i])][j]) == False or primefactors[str(num[i])].count(primefactors[str(num[i])][j]) > multiply[primefactors[str(num[i])][j]]:
				multiply[primefactors[str(num[i])][j]] = primefactors[str(num[i])].count(primefactors[str(num[i])][j])

	for i in multiply.keys():
		lcm = lcm * int(i) ** multiply[i]
	
	return lcm

def ishappy(num):
	nums = str(num)
	total = 0

	for i in range(0, len(nums)):
		total += int(nums[i:i + 1]) ** 2

	if total == 1: return True
	elif total == 4: return False;
	else: return ishappy(total)

def isperfect(num):
	divisors = getdivisors(num, True, False)
	divisorsum = 0
	
	for i in range(0, len(divisors)):
		divisorsum += divisors[i]
	
	if divisorsum == num: return True
	else: return False

def ispalindrome(num):
	if str(num) == str(num)[::-1]: return True
	else: return False

def islychrel(num, iterations):
	nums = str(num)[::-1]
	total = int(nums) + num

	if ispalindrome(total): return False
	elif iterations >= 1: return islychrel(int(total), iterations - 1)
	else: return True

def ispandigital(num):
	nums = str(num)

	if len(nums) > 9 or not nums.count("0") == 0: return False
	else:
		for i in range(1, len(nums)):
			if not nums.count(str(i)) == 1: return False
		return True
