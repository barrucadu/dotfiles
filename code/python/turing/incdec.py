#!/usr/bin/python3

from turing import *

# incdec.py - a Turing machine to increment and decrement binary numbers.

# Alphabet:
#    0 = binary zero
#    1 = binary one
#    + = increment number
#    - = decrement number
#    / = no operation

# It is assumed that the starting tape position is the right end of the number

# Example:
#  '10+++-' = 2 + 3 - 1 = 4 (= '100////')

#### Begin machine definition ####
states      = ["ready", "inc", "dec", "grow", "reset", "done", "ovrflw"]
haltstates  = ["done", "ovrflw"]
alphabet    = ["0", "1", "+", "-", "/"]
transitions = {"ready-1" : ["1", "ready",  "R"],
               "ready-0" : ["0", "ready",  "R"],
               "ready-/" : ["/", "ready",  "R"],
               "ready-+" : ["/", "inc",    "L"],
               "ready--" : ["/", "dec",    "L"],
               "ready-"  : ["/", "done",   "L"],

               "inc-/"   : ["/", "inc",    "L"],
               "inc-0"   : ["1", "reset",  "R"],
               "inc-1"   : ["0", "inc",    "L"],
               "inc-"    : ["1", "reset",  "R"],

               "dec-/"   : ["/", "dec",    "L"],
               "dec-0"   : ["1", "dec",    "L"],
               "dec-1"   : ["0", "reset",  "R"],
               "dec-"    : ["1", "ovrflw", "R"],

               "reset-0" : ["0", "reset",  "R"],
               "reset-1" : ["1", "reset",  "R"],
               "reset-/" : ["/", "ready",  "R"],
               "reset-"  : ["/", "done",   "L"]}
#### End machine definition ####

tape = input("")

incdec = Turing(tape, states, haltstates, "ready", transitions, alphabet, 0)
incdec.run()

print("Input tape:   " + tape)
print("Output tape:  " + incdec.tape)
print("Last state:   " + incdec.states[incdec.mystate])
print("Symbols read: " + str(incdec.symbols))
