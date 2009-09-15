#!/usr/bin/env python3

# Note:
# All lambda expressions, even the outermost, must be surrounded by parentheses in the current implementation.

# Lambda:
# {args: [args], body: [body]}

# Body:
# [symbol, symbol, symbol]

# Symbol:
# {lambda}, [expression], term

import sys

def parsestr (function):
    lambdas   = []
    bufferstr = ""
    depth     = 0
    for char in function:
        if not char == " ":
            if char == ")":
                depth -= 1
            
            if ((char == "(" or char == ")") and depth > 0) or not (char == "(" or char == ")"):
                bufferstr += char
                
            if char == "(":
                depth += 1
            
            if depth == 0 and len(bufferstr) > 0:
                lambdas.append(parselambda(bufferstr))
                bufferstr = ""

    return lambdas

def parselambda (lambdastr):
    args = []
    body = ""
    
    if not lambdastr[0] == "λ":
        print("Not a lambda expression:", lambdastr)
        sys.exit()
    else:
        lambdastr = lambdastr[1:]
        bits = lambdastr.split(".", 1)
        for arg in bits[0]:
            args.append(arg)
        body = parsebody(bits[1]);

    lam = {"args": args, "body": body}
    return lam

def parsebody (bodystr):
    bufferstr = ""
    depth = 0
    output = []
    for char in bodystr:
        if not char == " ":
            if char == ")":
                depth -= 1
                
            if ((char == "(" or char == ")") and depth > 0) or not (char == "(" or char == ")"):
                bufferstr += char
                    
            if char == "(":
                depth += 1
            
            if depth == 0:
                output.append(parseterm(bufferstr))
                bufferstr = ""
    
    return output

def parseterm (termstr):
    if len(termstr) == 1:
        return termstr
    elif termstr[0] == "λ":
        return parselambda(termstr)
    else:
        return parsebody(termstr)

def solvelambda (expression, args):
    for arg in expression[0]["args"]:
        if arg in args:
            del args[arg]

    n = -1
    for lambdas in expression:
        if n > -1:
            args[expression[0]["args"][0]] = lambdas
            expression[0]["args"] = expression[0]["args"][1:]
        n += 1

    n = 0
    for symbol in expression[0]["body"]:
        if not type(symbol) is list and not type(symbol) is dict and symbol in args:
            expression[0]["body"][n] = args[symbol]
        elif type(symbol) is dict:
            expression[0]["body"][n] = solvelambda ([symbol], args)
        elif type(symbol) is list:
            expression[0]["body"][n] = replaceargs (symbol, args)
        n += 1

    expression = [expression[0]]

    if lambdain(expression[0]["body"]):
        print(expression[0]["body"])
        expression = solvelambda(expression[0]["body"], args)

    return expression

def replaceargs (phrase, args):
    n = 0
    for symbol in phrase:
        if not type(symbol) is list and not type(symbol) is dict and symbol in args:
            phrase[n] = args[symbol]
        elif type(symbol) is dict:
            phrase[n] = solvelambda ([symbol], args)
        elif type(symbol) is list:
            phrase[n] = replaceargs (symbol, args)
        n += 1        
    return phrase

def lambdain (list):
    print("List:", list)
    n = 0
    for item in list:
        print("Item:", item)
        if type(item) is dict and n < len(list) - 1:
            return True
        n += 1
    return False

#lamexpr = "(λmnfx.m f (n f x))"
lamexpr = "(λmnfx.m f (n f x)) (λfx.x) (λfx.f(fx))"
#lamexpr = "(λf.(λx.fx))"
lam     = parsestr(lamexpr)

print("Original expression:", lamexpr)
print("Parsed expression:",   lam)

# Right, so I now have a fully parsed lambda expression.

solution = solvelambda(lam, {})

print("Solution:", solution)
