#!/usr/bin/env python

# Simple artificially intelligent maze-solving program
# Solves mazes by trying every route it encounters, but
# makes use of swarm AI techniques to minimize the time
# taken to do so.
# A wall is a hash, and a space is walkable terrain.
# The start is denoted by a S, and the finish by an F. A
# maze must be a quadrilateral

import sys

mfile = open('maze.txt')
maze = mfile.read()
maze = maze.splitlines()
mfile.close()

start = False

for i in range(0, len(maze)):
	for j in range(0, len(maze[i])):
		if maze[i][j] == "S":
			start = (j, i)
			break
	if not start == False: break

class Mazer:
	memory = []
	maze = []
	
	def __init__(self, memory, maze):
		self.memory = memory
		self.maze = maze
	
	def step(self, curx, cury):
		if self.maze[cury][curx] == "F":
			print self.memory
			print "Steps:", len(self.memory)
			sys.exit()
		else:
			lookup = True
			lookleft = True
			lookdown = True
			lookright = True
			
			if cury == 0: lookup = False
			if curx == 0: lookleft = False
			if cury == len(maze) - 1: lookdown = False
			if curx == len(maze[0]) - 1: lookright = False
			
			look = []
			
			if lookup == True: look.append((curx, cury - 1))
			if lookleft == True: look.append((curx - 1, cury))
			if lookdown == True: look.append((curx, cury + 1))
			if lookright == True: look.append((curx + 1, cury))
			
			for coords in look:
				if not self.maze[coords[1]][coords[0]] == "#" and self.memory.count(coords) == False:
					newmemory = []
					for item in self.memory:
						newmemory.append(item)
					newmemory.append(coords)
					newmazer = Mazer(newmemory, self.maze)
					newmazer.step(coords[0], coords[1])
			del self

mazer = Mazer([start], maze)
mazer.step(start[0], start[1])
