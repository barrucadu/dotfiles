# Data file
# ---------
#
# Contains all the data pertaining to the graph in the class Data.
# All data should go in *here*, rather than the main code file,
# to make it easier to adapt to other schools.

# os.path.join() should be used to create OS-independent
# file paths.

import os

class Data():
    # The vertices dictionary is in the form
    # 'vertex: {'region': (x1, y1, x2, y2), 'video': file',
    # where the coordinates specify a rectangular region
    # that, when clicked, sets the start/end point
    # of the path (as appropriate) to the vertex.
    # Arcs are drawn from the centre of these regions.
    # If the video file is None, no video is played.
    
    vertices  = {'a' : {'region' : (20,  16,  85,  81),  'video' : None},
                 'b' : {'region' : (18,  323, 86,  388), 'video' : None},
                 'c' : {'region' : (565, 328, 621, 385), 'video' : None},
                 'd' : {'region' : (567, 17,  625, 75),  'video' : None},
                 'e' : {'region' : (269, 163, 339, 223), 'video' : None},
                 'f' : {'region' : (263, 21,  347, 71),  'video' : None},
                 'g' : {'region' : (255, 332, 348, 392), 'video' : None}}
    
    # The arcs list is an array of arrays containing the
    # ends of an arc and its length in the form '[v1, v2, len]'.
    
    arcs      = [['a', 'b', 2],
                 ['a', 'f', 1],
                 ['b', 'g', 1],
                 ['g', 'e', 1],
                 ['g', 'c', 1],
                 ['c', 'd', 2],
                 ['d', 'f', 1],
                 ['f', 'e', 1]]
    
    # The image string contains the relative path to the
    # image file displayed.
    
    image     = os.path.join('resources', 'map.png')
    
    # The caption string contains the title of the program
    # window.
    
    caption   = 'Example Map'
    
    # The linecol tuple contains the RGB colour code for the lines,
    # the linewidth integer contains the width of the lines, and
    # the linelen integer contains the length of each line bit, and
    # the linegap integer contains the length of the gap between bits.
    
    linecol   = (0, 0, 0)
    linewidth = 2
    linelen   = 24
    linegap   = 16
    
    # The circlecol tuple contains the RGB colour code for the circles,
    # the circlewid integer contains the width of the circle border
    # (0 to fill), and the circlerad integer contains the radius of each circle.
    
    circlecol = (100, 100, 100)
    circlerad = 8
    circlewid = 0
