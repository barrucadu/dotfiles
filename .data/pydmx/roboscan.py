name = "Martin Roboscan 812"

# dmx = {"command" : {"argument" : [value, channeloffset, meaning, max], ...}, ...}

# If a max value is specified, a range is allowed.
# When a range, the value specified is the minimum value

dmx = {"setup"  : {"no"     : [0,   0, "No strobe"],
                   "gobo"   : [11,  0, "Gobo strobe",   74],
                   "colour" : [75,  0, "Colour strobe", 138],
                   "auto"   : [139, 0, "Remote auto trig"],
                   "music"  : [171, 0, "Remote music trig"],
                   "reset"  : [203, 0, "Reset fixture"],
                   "lamp"   : [236, 0, "Lamp power on"]},
       
       "colour" : {"b-w"   : [0,   1, "Black to white"],
                   "w-1"   : [15,  1, "White to colour 1"],
                   "1-2"   : [30,  1, "Colour 1 to colour 2"],
                   "2-3"   : [45,  1, "Colour 2 to colour 3"],
                   "3-4"   : [60,  1, "Colour 3 to colour 4"],
                   "4-5"   : [75,  1, "Colour 4 to colour 5"],
                   "5-6"   : [90,  1, "Colour 5 to colour 6"],
                   "6-7"   : [105, 1, "Colour 6 to colour 7"],
                   "7-8"   : [120, 1, "Colour 7 to colour 8"],
                   "8-9"   : [135, 1, "Colour 8 to colour 9"],
                   "9-10"  : [150, 1, "Colour 9 to colour 10"],
                   "10-11" : [165, 1, "Colour 10 to colour 11"],
                   "11"    : [180, 1, "Colour 11"],
                   "10"    : [185, 1, "Colour 10"],
                   "9"     : [190, 1, "Colour 9"],
                   "8"     : [195, 1, "Colour 8"],
                   "7"     : [200, 1, "Colour 7"],
                   "6"     : [205, 1, "Colour 6"],
                   "5"     : [210, 1, "Colour 5"],
                   "4"     : [215, 1, "Colour 4"],
                   "3"     : [220, 1, "Colour 3"],
                   "2"     : [225, 1, "Colour 2"],
                   "1"     : [230, 1, "Colour 1"],
                   "w"     : [235, 1, "White"],
                   "b"     : [240, 1, "Black"]},

       "gobo"   : {"c-o"   : [0,   2, "Closed to open"],
                   "o-1"   : [15,  2, "Open to gobo 1"],
                   "1-2"   : [30,  2, "Gobo 1 to gobo 2"],
                   "2-3"   : [45,  2, "Gobo 2 to gobo 3"],
                   "3-4"   : [60 , 2, "Gobo 3 to gobo 4"],
                   "4-5"   : [75,  2, "Gobo 4 to gobo 5"],
                   "5-6"   : [90,  2, "Gobo 5 to gobo 6"],
                   "6-7"   : [105, 2, "Gobo 6 to gobo 7"],
                   "7-8"   : [120, 2, "Gobo 7 to gobo 8"],
                   "8-9"   : [135, 2, "Gobo 8 to gobo 9"],
                   "9-10"  : [150, 2, "Gobo 9 to gobo 10"],
                   "10-11" : [165, 2, "Gobo 10 to gobo 11"],
                   "11"    : [180, 2, "Gobo 11"],
                   "10"    : [185, 2, "Gobo 10"],
                   "9"     : [190, 2, "Gobo 9"],
                   "8"     : [195, 2, "Gobo 8"],
                   "7"     : [200, 2, "Gobo 7"],
                   "6"     : [205, 2, "Gobo 6"],
                   "5"     : [210, 2, "Gobo 5"],
                   "4"     : [215, 2, "Gobo 4"],
                   "3"     : [220, 2, "Gobo 3"],
                   "2"     : [225, 2, "Gobo 2"],
                   "1"     : [230, 2, "Gobo 1"],
                   "o"     : [235, 2, "Open"],
                   "c"     : [240, 2, "Closed"]},

       "move"   : {"pan"  : [0, 3, "Pan",  255],
                   "tilt" : [0, 4, "Tilt", 255]},
       
       "speed"  : {"move" : [0, 5, "Movement speed",    255],
                   "col" :  [0, 6, "Colour/gobo speed", 255]}}

channels = ["Control", "Colour","Gobo", "Pan", "Tilt", "Move Speed", "Col. Speed"]
