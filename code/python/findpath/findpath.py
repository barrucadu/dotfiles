#!/usr/bin/env python

import pygame
#import pymedia
import sys
import math
from   pygame.locals import *
from   datafile      import Data

screen  = None
bgimage = None

def debug (message):
    # Don't want to print anything to STDOUT unless in debug mode.
    if 'debug' in sys.argv:
        if sys.version[0] == '3': # Despite there likely being other things which break in Python 3,
            print(message)        # this is a step in the right direction for cross-compatability between
        else:                     # 2.6, 3, Linux, and Windows.
            print message

def loadimage (path):
    try:
        image = pygame.image.load (path)
    except pygame.error, message:
        debug ("Cannot load image: " + path)
        sys.exit (1) # Not being able to load the map image is bad...
        
    return image, image.get_rect ()

def drawgui ():
    global screen
    global bgimage
    
    pygame.init ()

    # Load the background, magic up a window, and turn the image into something we can use.
    bgimage, size = loadimage (Data.image)
    screen        = pygame.display.set_mode ((size[2], size[3]))
    bgimage       = bgimage.convert ()

    # Then add a caption and draw the background
    pygame.display.set_caption (Data.caption)
    screen.blit (bgimage, (0, 0))
    pygame.display.flip ()

def showarc (start, end):
    global screen

    # These lines aren't very nice... calculate the centre of each region to use as the start/end points for the drawn arc.
    start_pos = ((Data.vertices[start]['region'][0] + Data.vertices[start]['region'][2]) / 2, (Data.vertices[start]['region'][1] + Data.vertices[start]['region'][3]) / 2)
    end_pos   = ((Data.vertices[end]['region'][0]   + Data.vertices[end]['region'][2])   / 2, (Data.vertices[end]['region'][1]   + Data.vertices[end]['region'][3])   / 2)

    # We also need to know *how* to draw the arc, and a better solution escapes me for now.
    x_big   = start_pos[0]
    x_small = end_pos[1]
    y_big   = start_pos[1]
    y_small = end_pos[1]

    if end_pos[0] > x_big:
        x_big   = end_pos[0]
        x_small = start_pos[0]

    if end_pos[1] > y_big:
        y_big   = end_pos[1]
        y_small = start_pos[1]

    length = int (math.floor (math.sqrt ((start_pos[0] - end_pos[0]) ** 2 + (start_pos[1] - end_pos[1]) ** 2)))
    chunks = int (math.floor (float (length) / (Data.linelen + Data.linegap)))

    bits = []
    if y_big - y_small < 16:
        # Roughly horizontal line
        for i in range (0, chunks):
            y = start_pos[1]
            if i * (Data.linelen + Data.linegap) > length / 2:
                y = end_pos[1]
                
            x1 = x_big - i * (Data.linelen + Data.linegap)
            x2 = x1 - Data.linegap
            
            bits.append ([(x1, y), (x2, y)])
    else:
        # Roughly vertical line
        for i in range (0, chunks):
            x = start_pos[0]
            if i * (Data.linelen + Data.linegap) > length / 2:
                x = end_pos[0]
                
            y1 = y_small + i * (Data.linelen + Data.linegap)
            y2 = y1 + Data.linegap
            
            bits.append ([(x, y1), (x, y2)])
    
    # Now draw all of the calculated line segments. Would be nice to have them drawn
    # in the correct order, so a slight delay could be added to make it look like steps.
    for bit in bits:
        pygame.draw.line (screen, Data.linecol, bit[0], bit[1], Data.linewidth)
        
    # Draw a blob at the start and end nodes for every arc; this will result in all
    # vertices passed having a blob.
    pygame.draw.circle (screen, Data.circlecol, start_pos, Data.circlerad, Data.circlewid)
    pygame.draw.circle (screen, Data.circlecol, end_pos,   Data.circlerad, Data.circlewid)

def playvideo (file):
    # To do: write this function.
    # Idea:  use pymedia.
    None
    
def drawpath (path):
    global screen
    global bgimage
    
    # Firstly, clear the screen by redrawing the background
    screen.blit (bgimage, (0, 0))
    pygame.display.flip ()
    
    # Then, draw every arc passed on the way to the destination.
    for i in range (0, len (path) - 1):
        showarc(path[i], path[i + 1])

    # Remember to update the display
    pygame.display.flip()


def getvertex ():
    # Here is where we try to translate a mouse click to a vertex. Try being the operative word.
    vertex = None
    x, y   = pygame.mouse.get_pos ()
    
    for vert in Data.vertices.keys ():
        # Check if the coordinates are within any of the given regions and, if so, return that vertex.
        # As you can see, in overlapping regions, the last specified one would be returned. But overlapping
        # regions would be silly.
        if x >= Data.vertices[vert]['region'][0] and x <= Data.vertices[vert]['region'][2] \
               and y >= Data.vertices[vert]['region'][1] and y <= Data.vertices[vert]['region'][3]:
            vertex = vert
    
    # Many debug messages are good.
    if not vertex == None:
        debug ("Clicked " + vertex)
        
    return vertex

def getarcs (vertex):
    # Get the arcs connected to a certain vertex. Does not matter if the vertex is
    # first or second in the list.
    arcs = []
    for arc in Data.arcs:
        if arc[0] == vertex:
            arcs.append ([arc[1], arc[2]])
        elif arc[1] == vertex:
            arcs.append ([arc[0], arc[2]])

    return arcs

def findpath (source, target):
    # This is more or less an implementation of Dijkstra's algorithm.
    # Probably best to read up on how that algorithm works if this doesn't
    # make much sense.
    graphinfo = {}
    for vertex in Data.vertices.keys ():
        graphinfo.update ({vertex: {'visited': False, 'order': -1, 'shortest': -1, 'last' : ''}})

    current = source

    graphinfo[current]['visited']  = True
    graphinfo[current]['order']    = 0
    graphinfo[current]['shortest'] = 0

    looping = True
    count   = 0

    while not current == target:
        count  += 1

        for arc in getarcs (current):
            if not graphinfo[arc[0]]['visited'] and (graphinfo[arc[0]]['shortest'] == -1 or arc[1] + graphinfo[current]['shortest'] < graphinfo[arc[0]]['shortest']):
                graphinfo[arc[0]]['shortest'] = arc[1] + graphinfo[current]['shortest']
                graphinfo[arc[0]]['last']     = current

        vertex   = ''
        shortest = -1
        for arc in graphinfo.keys ():
            if not graphinfo[arc]['visited']:
                if not graphinfo[arc]['shortest'] == -1 and (shortest == -1 or graphinfo[arc]['shortest'] < shortest):
                    shortest = graphinfo[arc]['shortest']
                    vertex   = arc

        graphinfo[vertex]['visited'] = True
        graphinfo[vertex]['order'] = count
        current = vertex

    solved   = False
    solution = [target]
    current  = target
    pathlen  = graphinfo[target]['shortest']
    curlen   = 0
    while not current == source:
        current = graphinfo[current]['last']
        solution.append (current)

    solution.reverse ()

    return solution, pathlen

# Now to run all those lovely functions we specified above. Begin!
drawgui ()

clock = pygame.time.Clock ()
start = None
end   = None

while True:
    clock.tick (30) # 30 fps limit. In fact, that might be a bit high for a simple thing like this.
    for event in pygame.event.get ():
        if event.type == KEYDOWN and event.key == K_ESCAPE:
            # On escape press, reset the start/end vertices, and print the mouse position.
            start = None
            end   = None
            debug (pygame.mouse.get_pos ())
        elif event.type == MOUSEBUTTONDOWN:
            # On click, any click, set either the start or end vertex.
            if start == None:
                start = getvertex ()
            elif end == None:
                end   = getvertex ()
                
    # If we have a start and end vertex, everything's shiny.
    if not start == None and not end == None:
        if not start == end:
            # If they're not the same (displaying a video for a useless route would be silly),
            # find a route, draw it, and play the video if there is one.
            path, length = findpath (start, end)
            debug ((path, length))
            drawpath (path)
            
            if not Data.vertices[end]['video'] == None:
                showvideo (Data.vertices[end]['video'])
                
        # Set the new start vertex to the old end, and the end to None.
        start = end
        end   = None
