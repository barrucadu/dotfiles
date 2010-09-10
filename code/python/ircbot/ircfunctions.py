import os, string, sys, urllib, re
msgbuff = {}

def respond(line, starti, pref, owner, speaker, s):
	global msgbuff
	startargs = starti + 1
	endargs = len(line)

	if line[len(line) - 2] == "chan":
		channel = line[len(line) - 1]
		endargs = len(line) - 2
	elif line[2] == "Barbot":
		channel = speaker
	else:
		channel = line[2]
	
	if len(line) > starti and line[starti] == pref + "part":
		if owner == speaker:
			s.send("PART %s\r\n" % channel)
		else:
			s.send("PRIVMSG %s You're not my daddy!\r\n" % channel)
	elif len(line) > starti and line[starti] == pref + "quit":
		if owner == speaker:
			message = ""
			i = startargs
			while i < endargs:
				message = message + line[i] + " "
				i += 1
			s.send("QUIT %s %s\r\n" % (channel, message))
			sys.exit()
		else:
			s.send("PRIVMSG %s You're not my daddy!\r\n" % channel)
	elif len(line) > starti and line[starti] == pref + "join":
		if owner == speaker:
			s.send("JOIN %s\r\n" % channel)
		else:
			s.send("PRIVMSG %s You're not my daddy!\r\n" % channel)
	elif len(line) > starti and line[starti] == pref + "say":
		if owner == speaker:
			message = ""
			i = startargs
			while i < endargs:
				message = message + line[i] + " "
				i += 1
			s.send("PRIVMSG %s %s\r\n" % (channel, message))
		else:
			s.send("PRIVMSG %s You're not my daddy!\r\n" % channel)
	elif len(line) > starti and line[starti] == pref + "act":
		if owner == speaker:
			message = ""
			i = startargs
			while i < endargs:
				message = message + line[i] + " "
				i += 1
			s.send("PRIVMSG %s :\1ACTION %s\1\r\n" % (channel,  message))
		else:
			s.send("PRIVMSG %s You're not my daddy!\r\n" % channel)
	elif len(line) > starti and line[starti] == pref + "syscmd":
		if owner == speaker:
			message = ""
			i = startargs
			while i < endargs:
				message = message + line[i] + " "
				i += 1
			out = syscmd(message)
			for outl in out:
				s.send("PRIVMSG %s %s\r\n" % (channel, outl))
		else:
			s.send("PRIVMSG %s You're not my daddy!\r\n" % channel)
	elif len(line) > starti and line[starti] == pref + "music":
		s.send("PRIVMSG %s Now listening to %s\r\n" % (channel, syscmd("mpc --format \"%title%, by %artist%\" | grep by")))
	elif len(line) > starti and line[starti] == pref + "nick":
		if owner == speaker:
			s.send("NICK %s\r\n" % channel)
		else:
			s.send("PRIVMSG %s You're not my daddy!\r\n" % channel)
	elif len(line) > starti and line[starti] == pref + "help":
		s.send("PRIVMSG %s I am controlled by typing \"!Barbot something (something )* (chan something)\". Commands are formed by modifications to these somethings.\r\n" % channel)
	elif len(line) > starti and line[starti] == pref + "aur":
		keywords = ""
		i = startargs
		isint = re.compile(r"^[-+]\d+$").match
		if isint(line[endargs - 1]):
			while i < endargs - 1:
				keywords = keywords + line[i] + " "
				i += 1
			packages = aursearch(keywords, int(line[endargs - 1]))
		else:
			while i < endargs:
				keywords = keywords + line[i] + " "
				i += 1
			packages = aursearch(keywords, 5)
		for package in packages.splitlines():
			s.send("PRIVMSG %s %s\r\n" % (channel, package))
	elif len(line) > starti and line[starti] == pref + "fortune":
		out = syscmd("fortune")
		for outl in out:
			s.send("PRIVMSG %s %s\r\n" % (channel, outl))
	elif len(line) > starti and line[starti] == pref + "art":
		out = syscmd("cat /home/barrucadu/ircbot/art.txt")
		for outl in out:
			s.send("PRIVMSG %s %s\r\n" % (channel, outl))
	elif len(line) > starti and line[starti] == pref + "shutdown":
		if owner == speaker:
			s.send("QUIT Bye bye.\r\n")
			sys.exit()
		else:
			s.send("PRIVMSG %s You're not my daddy!\r\n" % channel)
	elif not speaker == "nyx.devnode.org" and not speaker == "Barbot":
		if len(line) > starti:
			message = ""
			i = starti
			while i < endargs:
				message = message + line[i] + " "
				i += 1

			try:
				if len(msgbuff[channel]) == 4:
					msgbuff[channel][0] = msgbuff[channel][1]
					msgbuff[channel][1] = msgbuff[channel][2]
					msgbuff[channel][2] = msgbuff[channel][3]
				msgbuff[channel][len(msgbuff[channel]) - 1] = message
			except KeyError:
				msgbuff[channel] = [message]
				
			out = aisay(channel)
			if not out == False:
				s.send("PRIVMSG %s %s\r\n" % (channel, out))
#			else:
#				s.send("PRIVMSG %s What?\r\n" % channel)
#					
#		else:
#			s.send("PRIVMSG %s What?\r\n" % channel)

def syscmd(command):
	command = command.rstrip()
	os.system(command + ' >temp.txt')
	a = open('temp.txt')
	ot = a.read()
	ot = ot.splitlines()
	a.close()
	return ot

def aursearch(keywords, totalnum):
	search = "http://aur.archlinux.org/packages.php?O=0&L=0&C=0&K=" + keywords.replace(" ", "+") + "&SeB=nd&SB=n&SO=a&PP=25&do_Search=Go"
	urllib.urlretrieve(search, "aur.tmp")
	block = ""
	num = 0
	sect = 0
	packages = ""
	aur = open("aur.tmp")
	for aurl in aur:
		if num <= totalnum:
			if aurl[:20] == "  <td class='data1'>" or aurl[:20] == "  <td class='data2'>": print "aursearch", aurl
			
			if aurl[:20] == "  <td class='data1'>" and not block == "data1":
				sect = 0
				block = "data1"
			elif aurl[:20] == "  <td class='data2'>" and not block == "data2":
				sect = 0
				block = "data2"
			elif aurl[:20] == "  <td class='data1'>" or aurl[:20] == "  <td class='data2'>":
				print aurl
				sect += 1
				if sect == 2:
					pacname = aurl.split("<span class='black'>")
					pacname = pacname[1].split("</span>")
					pacname = pacname[0]
					packages = packages + pacname
				elif sect == 4:
					pacdesc = aurl.split("<span class='blue'>")
					pacdesc = pacdesc[1].split("</span>")
					pacdesc = pacdesc[0]
					packages = packages + " - " + pacdesc + "\n"
					num += 1
	aur.close()
	urllib.urlcleanup()
	return packages

def aisay(channel):
	global msgbuff
	print "aisay: ", msgbuff[channel]
	result = False
	if len(msgbuff[channel]) == 4 and not msgbuff[channel][0] == msgbuff[channel][1] and msgbuff[channel][1] == msgbuff[channel][2] and msgbuff[channel][2] == msgbuff[channel][3]:
		result = msgbuff[channel][1]
	return result
