#!/usr/bin/env python

import sys, socket, string, os, ircfunctions

HOST = "nyx.devnode.org"
PORT = 6667
NICK = "Barbot"
IDENT = "barbot"
REALNAME = "BarrucadusBot"
readbuffer = ""
owner = "Barrucadu"

s = socket.socket()
s.connect((HOST, PORT))
s.send("NICK %s\r\n" % NICK)
s.send("USER %s %s bla :%s\r\n" % (IDENT, HOST, REALNAME))
s.send("MODE Barbot B +")

while 1:
	try:
		readbuffer = readbuffer+s.recv(1024)
		temp = string.split(readbuffer, "\n")
		readbuffer = temp.pop()
		
		for line in temp:
			line = string.rstrip(line)
			print line
			line = string.split(line)
			starti = 0
			parsel = False
			pref = ""
			
			if line[0] == "PING":
				s.send("PONG %s\r\n" % line[1])
			elif len(line) > 3 and (line[3] == ":!Barbot" or line[3] == ":!barbot"):
				starti = 4
				parsel = True
			elif len(line) > 2 and (line[2] == "Barbot" or line[2] == "barbot"):
				starti = 3
				parsel = True
				pref = ":"
			
			if parsel == True:

				nick = line[0].split('!')
				nick = nick[0].split(':')
				speaker = nick[1]
				
				if len(line) > starti and line[starti] == pref + "reload":
					if owner == speaker:
						reload(ircfunctions)
						s.send("PRIVMSG Barrucadu Configuration reloaded.\r\n")
						print "Configuration reloaded."
				else:
					ircfunctions.respond(line, starti, pref, owner, speaker, s)
	except KeyboardInterrupt:
		s.send("QUIT Bye bye.")
		sys.exit()
