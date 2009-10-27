#!/usr/bin/env python

# Todo: Accept list of desired items
#       Calculate total price for items
#       Calculate price using any offers (this requires some wildcard support in offers)
#       Suggest any offers which can almost be applied

import sys, os

datapath = ""

if len(sys.argv) == 2:
    if os.path.isfile("/home/barrucadu/code/python/takeaway/" + sys.argv[1]):
        datapath = "/home/barrucadu/code/python/takeaway/" + sys.argv[1]
    elif os.path.isfile(sys.argv[1]):
        datapath = sys.argv[1]

if datapath == "":
    print "Data file not found."
    sys.exit()

items    = []
offers   = []
order    = []
initems  = False
inoffers = False

datafile = open(datapath)
for line in datafile:
    line = line.strip()
    if not line == "":
        if not initems and line == "Items":
            initems  = True
            inoffers = False
        elif not inoffers and line == "Offers":
            initems  = False
            inoffers = True
            
        if initems and not line == "Items":
            splitted = line.split(" ", 1)
            items.append({"item" : splitted[1], "price" : splitted[0]})
        elif inoffers and not line == "Offers":
            splitted      = line.split(" ", 1)
            splitteditems = splitted[1].split(" / ")
            offers.append({"items" : splitteditems, "price" : splitted[0]})

print items
print offers
