#!/usr/bin/python3

from turing import *

# incdec.py - a Turing machine to increment and decrement binary numbers.

# Alphabet:
#    0 = binary zero
#    1 = binary one
#    > = lower end of number
#    < = upper end of number
#    + = increment number
#    - = decrement number
#    / = no operation
#    # = halt

# It is assumed that the starting tape position is the index of the '>'

# Example:
#  '<10>+++-#' = 2 + 3 - 1 = 4 (= '<100>////#')

#### Begin machine definition ####
states      = ["ready", "inc", "dec", "grow", "reset", "halt"]
haltstates  = ["halt"]
alphabet    = ["0", "1", "<", ">", "+", "-", "/", "#"]
transitions = {"ready->" : [">", "ready", "R"],
               "ready-/" : ["/", "ready", "R"],
               "ready-#" : ["#", "halt",  "L"],
               "ready-+" : ["/", "inc",   "L"],
               "ready--" : ["/", "dec",   "L"],

               "inc->"   : [">", "inc",   "L"],
               "inc-<"   : ["1", "grow",  "L"],
               "inc-/"   : ["/", "inc",   "L"],
               "inc-0"   : ["1", "reset", "R"],
               "inc-1"   : ["0", "inc",   "L"],

               "dec->"   : [">", "dec",   "L"],
               "dec-<"   : ["<", "halt",  "R"],
               "dec-/"   : ["/", "dec",   "L"],
               "dec-0"   : ["1", "dec",   "L"],
               "dec-1"   : ["0", "reset", "R"],

               "grow-"   : ["<", "reset", "R"],

               "reset-0" : ["0", "reset", "R"],
               "reset-1" : ["1", "reset", "R"],
               "reset->" : [">", "ready", "R"]}
#### End machine definition ####

tape = "<10>+++-#"

incdec = Turing(tape, states, haltstates, "ready", transitions, alphabet, tape.index(">"))
incdec.run()

print("Input tape:   " + tape)
print("Output tape:  " + incdec.tape)
print("Last state:   " + incdec.states[incdec.mystate])
print("Symbols read: " + str(incdec.symbols))
