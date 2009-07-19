#!/usr/bin/env python

# A script to parse the MPD database into a list of dictionaries.
# Now with patronising comments which assume almost no Python knowledge!

# A nice little help function. Read on to see how it is called...
def showhelp():
    print "Usage: mpdspl [options]\n"
    print "A script to generate smart playlists for MPD. Currently does nothing of use :p\n"
    print "Options:"
    print "    -f, --force           - Force an update of the cache file."
    print "    -dFILE, --dbpath=FILE - Location of the database file."
    print "    -h, --help            - Display this text and exit."
    sys.exit()

# Splitting things up into functions is good :D
def parseargs():
    # global lets us access variables specified outside our function.
    global forceupdate
    global dbpath
    
    for argument in sys.argv:
        if argument == "-f" or argument == "--force":
            # If a "-f" or "--force" parameter is sent, force the cache to be updated even if it doesn't look like it needs to be.
            forceupdate = True
        elif argument[:2] == "-d" or argument[:9] == "--dbpath=":
            if argument[:2] == "-d":
                # However, Python can't work with ~, which has a reasonable chance of being used (eg: ~/.mpd/mpd.db"), so it needs to be expanded.
                dbpath = os.path.expanduser(argument[2:])
            elif argument[:9] == "--dbpath=":
                dbpath = os.path.expanduser(argument[9:])
        elif argument == "-h" or argument == "--help":
            showhelp()
        elif not argument == sys.argv[0]: # The first argument is the filename. Don't complain about not understanding it...
            # Ooh, stderr. I never actually knew how to send stuff through stderr in python.
            print >> sys.stderr, "Unrecognised parameter '" + argument + "'"
            sys.exit(1)

# cPickle is a faster version of the pickle library. It is used to save data structures to a file. Like lists and dictionaries. os is needed for file stuff, sys for arguments.
import cPickle, os, sys

# Default place to look for MPD database. If a -d option is specified, look there instead.
dbpath    = "/var/lib/mpd/mpd.db"

# There is an environmental variable XDG_CACHE_HOME which specifies where to save cache files. However, if not set, a default of ~/.cache should be used.
cachehome = os.path.expanduser(os.environ['XDG_CACHE_HOME'])
if cachehome == "":
    cachehome = os.environ['HOME'] + "/.cache/"
cachepath = cachehome + "/mpdspl/mpddb.cache"

# $XDG_DATA_HOME which specifies where to save data files. Like a record of playlists which have been created. If unset a default of ~/.local/share should be used. This is currently unused as there is no actual creation of playlists yet :p
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
