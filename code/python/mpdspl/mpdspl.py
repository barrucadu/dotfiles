#!/usr/bin/env python

# A script to parse the MPD database into a list of dictionaries.
# Now with patronising comments which assume almost no Python knowledge!

# cPickle is a faster version of the pickle library. It is used to save data structures to a file. Like lists and dictionaries. os is needed for file stuff, sys for arguments, and re for regex.
import cPickle, os, sys, re

# A nice little help function. Read on to see how it is called...
def showhelp():
    print "Usage: mpdspl [options]\n"
    print "A script to generate smart playlists for MPD. Currently does nothing of use :p\n"
    print "Options:"
    print "    -f, --force              - Force an update of the cache file."
    print "    -dFILE, --dbpath=FILE    - Location of the database file."
    print "    -h, --help               - Display this text and exit."
    print "    -n, --new [name] [rules] - Create a new playlist.\n"
    print "Playlist rules:"
    print "    These are specified as a string of Python-compatible regular expressions separated by keywords, spaces, and slashes. They are matched by re.search, not re.match, and no special flags are passed, other than re.IGNORECASE when requested."
    print "    These keywords are:"
    print "        ar = Artist"
    print "        al = Album"
    print "        ti = Title"
    print "        tr = Track Number"
    print "        ge = Genre"
    print "        ye = Year"
    print "        le = Length (seconds)"
    print "        fp = File Path (relative to MPD root dir, including filename)"
    print "        fn = File Name\n"
    print "    Regular expressions are specified within slashes (/regex/), and if the first slash is preceeded by an 'i', the regular expression is interpreted as case-insensitive. If the final slash is succeeded by a 'n', the result of the match is negated.\n"
    print "    For example, a rule for all tracks by 'Fred' or 'George', which have a title containing (case insensitive) 'The' and 'and', but not 'when' would be:"
    print "        ar=/(Fred|George)/ ti=i/(the.*and|and.*the)/ ti=i/when/n"
    sys.exit()

# Parse the rules regex
def parserules(rulestr):
    # rules will be our list of rules, bufferstr will be the buffer for our parser, and i will be a counter
    rules     = []
    bufferstr = ""
    i         = 0

    # We want to use the same identifiers as the track dictionaries:
    keywords = {"ar" : "Artist", "al" : "Album", "ti" : "Title", "tr" : "Track", "ge" : "Genre", "ye" : "Date", "le" : "Time", "fp" : "key", "fn" : "file"}

    # For every character in rulestr (we do it characterwise, hence needing a buffer)
    for c in rulestr:
        # Add the character to the buffer
        bufferstr += c

        # If the buffer matches one of our keywords, we have hit a new rule, and so create a blank dictionary, and clear the buffer.
        if bufferstr.strip() in ["ar", "al", "ti", "tr", "ge", "ye", "le", "fp", "fn"]:
            rules.append({"type" : keywords[bufferstr.strip()], "regex" : "", "compiled" : None, "inverse" : False, "negate" : False})
            bufferstr = ""
        # If we're at the start of a blank case-insensitive regex, record that, and clear the buffer.
        elif bufferstr == "=i/":
            rules[i]["i"] = True
            bufferstr = ""
        # If not, just clear the buffer for the coming regex.
        elif bufferstr == "=/":
            bufferstr = ""
        # If at the end of a regex, stick it all (sans the trailing slash, they're just a nice separater for our parser) to the dictionary, increment the counter, and clear the buffer ready for the next rule.
        elif bufferstr[-1] == "/":
            rules[i]["regex"] = bufferstr[:-1]
            bufferstr = ""
            i += 1
        # If set to 'n' and the regex has been set, negate it.
        elif bufferstr == "n" and not rules[i - 1]["regex"] == "":
            bufferstr = ""
            rules[i - 1]["negate"] = True

    # This isn't needed. But it makes things faster and allows us to have case insensetivity.
    for rule in rules:
        regex = None
        if rule["inverse"]:
            # If case insensitive, compile it as such.
            regex = re.compile(rule["regex"], re.IGNORECASE)
        else:
            regex = re.compile(rule["regex"])

        # Overwrite the regex string with the compiled object
        rule["compiled"] = regex

    return rules

# Splitting things up into functions is good :D
def parseargs():
    # global lets us access variables specified outside our function.
    global forceupdate
    global dbpath
    global newname
    global newrules

    newarg = 0
    
    for argument in sys.argv:
        if not newarg == 0:
            # We're making a new playlist. If we're only on the first option after -n, that's the name. If the second, that's the description.
            if newarg == 2:
                newname = argument
            elif newarg == 1:
                newrules = parserules(argument)
            newarg -= 1
        else:
            if argument == "-f" or argument == "--force":
                # If a "-f" or "--force" parameter is sent, force the cache to be updated even if it doesn't look like it needs to be.
                forceupdate = True
            elif argument[:2] == "-d" or argument[:9] == "--dbpath=":
                # Looks like their db is somewhere other than /var/lib/mpd/mpd.db...
                if argument[:2] == "-d":
                    # Python can't work with ~, which has a reasonable chance of being used (eg: ~/.mpd/mpd.db"), so it needs to be expanded.
                    dbpath = os.path.expanduser(argument[2:])
                elif argument[:9] == "--dbpath=":
                    dbpath = os.path.expanduser(argument[9:])
            elif argument == "-n" or argument == "--new":
                # Do special treatment to the next 2 arguments
                newarg = 2
            elif argument == "-h" or argument == "--help":
                showhelp()
            elif not argument == sys.argv[0]: # The first argument is the filename. Don't complain about not understanding it...
                # Ooh, stderr. I never actually knew how to send stuff through stderr in python.
                print >> sys.stderr, "Unrecognised parameter '" + argument + "'"
                sys.exit(1)

# Info about new playlists
newname  = ""
newrules = []

# Default place to look for MPD database. If a -d option is specified, look there instead.
dbpath    = "/var/lib/mpd/mpd.db"

# There is an environmental variable XDG_CACHE_HOME which specifies where to save cache files. However, if not set, a default of ~/.cache should be used.
cachehome = os.path.expanduser(os.environ['XDG_CACHE_HOME'])
if cachehome == "":
    cachehome = os.environ['HOME'] + "/.cache/"
cachepath = cachehome + "/mpdspl/mpddb.cache"

# $XDG_DATA_HOME specifies where to save data files. Like a record of playlists which have been created. If unset a default of ~/.local/share should be used. This is currently unused as there is no actual creation of playlists yet :p
datahome = os.path.expanduser(os.environ['XDG_DATA_HOME'])
if datahome == "":
    datahome = os.environ['HOME'] + "/.local/share/"
datapath = cachehome + "/mpdspl/"
# If the data directory does not exist, create it.
if not os.path.isdir(datapath):
    os.mkdir(datapath)

tracks    = []

forceupdate = False

parseargs()

# Check that the database is actually there before attempting to do stuff with it.
if not os.path.exists(dbpath):
    print >> sys.stderr, "The database file '" + dbpath + "' could not be found."
    sys.exit(1)

# If the cache file does not exist OR the database has been modified since the cache file has this has the side-effect of being able to touch the cache file to stop it from being updated. Good thing we have the -f option for any accidental touches (or if you copy the cache to a new location).
if not os.path.exists(cachepath) or os.path.getmtime(dbpath) > os.path.getmtime(cachepath) or forceupdate:
    print "Updating database cache..."

    # If the cache directory does not exist, create it. The dirname function just removes the "/mpddb.cache" from the end.
    if not os.path.isdir(os.path.dirname(cachepath)):
        os.mkdir(os.path.dirname(cachepath))

    # Open the cache file for writing, and the database file for reading
    cachefile = open(cachepath, "wb")
    database  = open(dbpath, "r")
    i         = -1
    parsing   = False
    
    for line in database:
        # For every line in the database, remove any whitespace at the beginning and end so the script isn't buggered.
        line = line.strip()

        # If entering a songList, start parsing. If exiting one, stop. Fairly self explanatory.
        if not parsing and line == "songList begin":
            parsing = True
        elif parsing and line == "songList end":
            parsing = False

        # If we get a line to parse which is not a "songList begin" statement (because it's be stupid to do things with that)
        if parsing and not line == "songList begin":
            if line[0:5] == "key: ":
                i += 1
                # Increment the counter and make an empty dictionary if we hit the beginning of a track
                tracks.append({"key" : "", "file" : "", "Time" : "", "Genre" : "", "Title" : "", "Artist" : "", "Date" : "", "Album" : "", "Track" : "", "mtime" : ""})

            # Split the line by the first ": ", the string MPD uses, and stick the second part (the value) in the bit of the dictionary referred to by the first part (the key)
            splitted = line.split(": ", 1)
            tracks[i][splitted[0]] = splitted[1]

    # Save the parsed stuff to the cache file and close the file handlers. That's not strictly required, python will clean up when the script ends, but you can't unmount volumes with file handlers pointing to them, so it makes a mess.
    cPickle.dump(tracks, cachefile)
    database.close()
    cachefile.close()
else:
    # Oh, goodie, we don't need to go through all that arduous parsing as we have a valid cache file :D
    print "Loading database cache..."
    # Open it for reading, load the stuff in the file into the tracks list, close the file handler, and have a party.
    cachefile = open(cachepath, "rb")
    tracks = cPickle.load(cachefile)
    cachefile.close()

# See if we're making a new playlist or not
if not newname == "":
    # matchingtracks will hold all tracks which match all of the criteria.
    matchingtracks = []
    
    for track in tracks:
        # Initially assume a track *will* be added.
        addtrack = True
        
        for rule in newrules:
            # For every track, check it with every rule
            if rule["negate"]:
                if not re.search(rule["compiled"], track[rule["type"]]) == None:
                    # If the regular expression matches the track, do not add it to the matchingtracks list.
                    addtrack = False
            else:
                if re.search(rule["compiled"], track[rule["type"]]) == None:
                    # If the regular expression does not match the track, do not add it to the matchingtracks list.
                    addtrack = False
        
        if addtrack:
            # Add the track if appropriate
            matchingtracks.append(track)

print "To do list:"
print "    Actually create a playlist, don't just find matching tracks."
print "    Save rules and suchlike to somewhere in datapath. I'm unsure if you can save compiled regular expressions, so experiment."
print "    Split database parsing and cache loading into two separate functions."
print "    Add an update flag to update all generated playlists."
print "    Anything else you can think of."
