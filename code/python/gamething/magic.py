import sys

magiccircle = 0
spellbook = []

def magic_circle (circle):
    global magiccircle
    magiccircle = circle
    
    if circle == 0:
        print "You dispell the circle with a wave of your hand."
    elif circle == 1:
        print "You cast a basic magic circle."
    elif circle == 2:
        print "You cast a complex magic circle."
    else:
        print "You cast an organe magic circle."

def create_spell (spellstr):
    global spellbook
    global magiccircle
    
    spell = []
    magics = []

    # Pass One: make magic pairs
    curelement = spellstr[0]
    curcount   = 1
    inpair = 0
    for i in range (1, len (spellstr)):
        if spellstr[i] in ("a", "e", "f", "w"):
            if spellstr[i] == curelement:
                curcount += 1
                if (curcount / 3.0) > magiccircle:
                    print "Your magic circle disintegrates, not powerful enough to contain the magic you are casting, and the spell collapses."
                    return
            else:
                if inpair == 0:
                    magics.append (str (curcount) + curelement)
                    inpair = 1
                else:
                    magics[-1] += str (curcount) + curelement
                    inpair = 0
                curelement = spellstr[i]
                curcount = 1
    if inpair == 0:
        magics.append (str (curcount) + curelement)
        inpair = 1
    else:
        magics[-1] += str (curcount) + curelement
        inpair = 0

    # Pass Two: fix any cancelling out that happens
    newmagics = []
    for pair in magics:
        if len(pair) == 4:
            if (pair[1] == "f" and pair[3] == "w") or (pair[1] == "w" and pair[3] == "f") or (pair[1] == "a" and pair[3] == "e") or (pair[1] == "e" and pair[3] == "a"):
                newpair = ""
                if int(pair[0]) > int(pair[2]):
                    newpair = str(int(pair[0]) - int(pair[2])) + pair[1]
                elif int(pair[0]) < int(pair[2]):
                    newpair = str(int(pair[2]) - int(pair[0])) + pair[3]
                pair = newpair
        newmagics.append(pair)
    magics = newmagics
    
    name = raw_input ("Name this spell: ")
    spell = [name, magics]
    spellbook.append (spell)
    return spell

while True:
    inputtxt = raw_input ("Enter a spell: ")
    if len (inputtxt) > 0:
        if inputtxt[0] == "c":
            magic_circle (len (inputtxt))
        elif inputtxt == "e":
            print spellbook
            print magiccircle
            sys.exit ()
        else:
            print create_spell (inputtxt)
