#!/usr/bin/python3

import sys

# turing.py - generic implementation of a Turing machine.

class Turing:
    states      = [] # A list of state names
    haltstates  = [] # If a state is reached which is also in this list excuion halts.
    transitions = {} # A dictionary in the form 'state-symbol' : [newsymbol, newstate, direction]
                     # The key 'state-' matches the blank symbol in the given state.
                     # The direction is one of 'L' or 'R'
    alphabet    = [] # The allowed symbols for this machine
    mystate     = 0  # Index number of current state
    tape        = "" # Current tape
    tapepos     = 0  # Position in the tape
    tapeblank   = "" # The symbol used to fill in the gap when extending the tape
    symbols     = 0  # Number of symbols read since starting the last tape

    def __init__(self, tape="", states=[], haltstates=["halt"], initialstate="", transitions={}, alphabet=[], tapepos=0, blanksymbol=" "):
        self.tape        = tape
        self.states      = states
        self.haltstates  = haltstates
        self.mystate     = states.index(initialstate)
        self.transitions = transitions
        self.alphabet    = alphabet
        self.tapepos     = tapepos
        self.tapeblank   = blanksymbol

        if not blanksymbol in alphabet:
            self.alphabet.append(blanksymbol)

    def moveLeft(self):
        if self.tapepos is 0:
            # We have to extend the tape to the left with a blank symbol
            self.tape = self.tapeblank + self.tape
        else:
            # Decrement the tapepos counter
            self.tapepos -= 1

    def moveRight(self):
        if self.tapepos is len(self.tape) - 1:
            # We have to extend the tape to the right with a blank symbol
            self.tape += self.tapeblank

        # Increment the tapepos counter
        self.tapepos += 1

    def doNextSymbol(self):
        symbol = self.tape[self.tapepos]
        state  = self.states[self.mystate]

        if not symbol in self.alphabet:
            print("Symbol '" + symbol + "' not found in alphabet. Terminating.")
            sys.exit(1)

        transition = state + "-"
        if not symbol == self.tapeblank: transition += symbol

        newsymbol = ""
        newstate  = ""
        direction = None

        if transition in self.transitions.keys():
            newsymbol = self.transitions[transition][0]
            newstate  = self.transitions[transition][1]
            direction = self.transitions[transition][2]
        else:
            print("Symbol '" + symbol + "' not found in transitions specification for state '" + state + "'. Terminating.")
            sys.exit(1)

        if not newsymbol in self.alphabet:
            print("Symbol '" + newsymbol + "' not found in alphabet. Terminating.")
            sys.exit(1)

        self.tape = self.tape[:self.tapepos] + newsymbol + self.tape[self.tapepos+1:]

        if newstate in self.states:
            self.mystate = self.states.index(newstate)
        else:
            print("State '" + newstate + "' not found in state specification. Terminating.")
            sys.exit(1)

        if direction is "L":
            self.moveLeft()
        elif direction is "R":
            self.moveRight()
        else:
            print("Direction '" + direction +"' unknown. Terminating.")
            sys.exit(1)

    def run(self):
        while not self.states[self.mystate] in self.haltstates:
            self.symbols += 1
            self.doNextSymbol()

