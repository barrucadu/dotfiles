#!/usr/bin/env python

from string import *

def getfloat(string, pos):
	gotfloat = ""
	for i in range(pos, len(string)):
		if (ord(string[i]) <= 57 and ord(string[i]) >= 48) or ord(string[i]) == 46:
			gotfloat += string[i]
		elif not gotfloat == "":
			return [gotfloat, i]
	return [gotfloat, i]

def getvar(string, pos):
	gotvar = ""
	for i in range(pos, len(string)):
		if (ord(string[i]) <= 90 and ord(string[i]) >= 65) or (ord(string[i]) <= 122 and ord(string[i]) >= 97):
			gotvar += string[i]
		elif not gotvar == "":
			return [gotvar, i]
	return [gotvar, i]

def getterms(equation):
	terms = []
	pastequals = False
	nextsign = 1
	spliteqn = split(equation)
	
	for i in range(0, len(spliteqn)):
		t = spliteqn[i]
		if pastequals == True:
			if not (t == "+" or t == "-"):
				a = getfloat(t, 0)
				x = getvar(t, a[1])
				n = 0

				if not t[x[1] + 1] == "-":
					n = getfloat(t, x[1])
					n = float(n[0])
				else:
					n = getfloat(t, x[1] + 2)
					n = float(n[0]) * -1

				a = float(a[0]) * nextsign
				x = x[0]

				terms.append([a, x, n])
			elif t == "+":
				nextsign = 1
			else:
				nextsign = -1
		elif t == "=":
			pastequals = True
			terms.append(spliteqn[0])
			
	return terms

def differentiatelevel(equation):
	terms = getterms(equation)
	differentiated = ""
	
	for i in range(1, len(terms)):
		t = terms[i]
		differentiated += replace(str(t[0] * t[2]), "-", "- ") + str(t[1]) + "^" + str(t[2] - 1) + " "
		if i < len(terms) - 1 and (terms[i + 1][0] * terms[i + 1][2]) >= 0:
			differentiated += "+ "

	return differentiated

def differentiate(equation, times):
	equationnew = differentiatelevel(equation)
	for i in range(1, times):
		equationnew = differentiatelevel("y = " + equationnew)

	return equationnew

def killc(equation, times):
	for i in range(0, times + 1):
		equation = replace(equation, "c_" + str(i), "")
	return equation

def addc(equation, times):
	for i in range(0, times - 1):
		equation += " + c_" + str(i)
	return equation

def integratelevel(equation, clevel):
	terms = getterms(replace(equation, "c", ""))
	integrated = ""
	
	for i in range(1, len(terms)):
		t = terms[i]
		integrated += replace(str(t[0] / (t[2] + 1)), "-", "- ") + str(t[1]) + "^" + str(t[2] + 1) + " "
		if i < len(terms) - 1 and (terms[i + 1][0] / (terms[i + 1][2] + 1)) >= 0:
			integrated += "+ "

	integrated += "+ c_" + str(clevel)
	return integrated

def integrate(equation, times):
	equationnew = integratelevel(equation, times - 1)
	for i in range(1, times):
		equationnew = integratelevel("y = " + killc(equationnew, times - i), times - i)

	return addc(equationnew, times)

print "Terms of y = 5x^3 + 3x^2 + 19x^-9"
print "     " + str(getterms("y = 5x^3 + 3x^2 + 19x^-9"))
print "\nFirst derivative of y = 5x^3 + 3x^2 + 19x^-9"
print "     dy/dx = " + differentiate("y = 5x^3 + 3x^2 + 19x^-9", 1)
print "\nSecond derivative of y = 5x^3 + 3x^2 + 19x^-9"
print "     d^y/dx^2 = " + differentiate("y = 5x^3 + 3x^2 + 19x^-9", 2)
print "\nFirst integral of y = 5x^3 + 3x^2 + 19x^-9"
print "     y = " + integrate("y = 5x^3 + 3x^2 + 19x^-9", 1)
print "\nSecond integral of y = 5x^3 + 3x^2 + 19x^-9"
print "     y = " + integrate("y = 5x^3 + 3x^2 + 19x^-9", 2)
