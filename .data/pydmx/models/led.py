name = "Stairville LED Par 56"

dmx = {"control"  : {"rgb"   : [0,   0, "RGB mode"],
                     "7colf" : [64,  0, "7 Colour Fade"],
                     "7colc" : [128, 0, "7 Colour Change"],
                     "3colc" : [192, 0, "3 Colour Change"]},
       
       "colour" : {"red"   : [0, 1, "Red"],
                   "green" : [0, 2, "Green"],
                   "blue"  : [0, 3, "Blue"]},
       
       "speed"  : {"nospeed"  : [0,   4, "No function - no speed"],
                   "manual"   : [11,  4, "Manual speed control"],
                   "nospeed2" : [101, 4, "No function - no speed"],
                   "unit"     : [151, 4, "Speed control by unit, music, or VAR"]}}

channels = ["Control", "Red", "Green", "Blue", "Speed"]

defaults = [0, 0, 0, 0, 0]
