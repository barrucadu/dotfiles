#!/usr/bin/env python

import socket
nyuserver = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
nyuserver.bind(('', 13020))
nyuserver.listen(1)
while True:
	channel, details = nyuserver.accept()
	print 'Connection from ', details
	channel.send ("Nyu!\n")
	channel.close()
