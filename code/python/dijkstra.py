#!/usr/bin/env python

class dijkstra:
	nodes    = []
	arcs     = []
	nodeinfo = []

	def addnode(node):
		if not node in self.nodes:
			nodeinfo = {"name"    : node,
						"p-label" : -1,
						"t-label" : 0}
			
			self.nodes.append(node)
			self.nodeinfo.append(nodeinfo)
			
			return True
		else:
			return False

	def addarc(start, end, directed):
		arc = {"start"    : start,
			   "end"      : end,
			   "directed" : directed}

		if not arc in self.arcs:
			self.arcs.append(arc)
			return True
		else:
			return False

	def cleargraph():
		self.nodes    = []
		self.arcs     = []
		self.nodeinfo = []

		return True

	def ispnode(node):
		isp = False
		for info in self.nodeinfo:
			if self.nodeinfo["name"] == node and not self.nodeinfo["p-label"] == -1:
				isp = True
				
		return isp

	def getconnections(node):
		connections = []
		for arc in self.arcs:
			arcinfo = {"name"   : "",
					   "weight" : 0}
			if arc.start == node and ispnode(arc.end) == False:
				arcinfo["name"] = arc.end
				connections.append(arcinfo)
			elif arc.end == node and arc.directed == False and ispnode(arc.start) == False:
				arcinfo["name"] = arc.start
				connections.append(arcinfo)
				
		return connections

	def getnextnode(node):
		connections = getconnections(node)
		nextnode    = ""
		curweight   = -1
		
		for arc in connection:
			if arc["weight"] < curweight or curweight == -1:
				nextnode  = arc["name"]
				curweight = arc["weight"]

		for inode in self.nodeinfo:
			if (inode["t-label"] < curweight or curweight == -1) and not ispnode(inode["name"]):
				nextnode  = inode["name"]
				curweight = inode["t-label"]

		return nextnode

	def updatenodes(node):
