#!/usr/bin/env python

import cPickle, os

dbpath    = "/var/lib/mpd/mpd.db"
cachepath = "/home/barrucadu/tmp/misc/mpddb.cache"
tracks    = []

if not os.path.isfile(cachepath) or os.path.getmtime(dbpath) > os.path.getmtime(cachepath):
    cachefile = open(cachepath, "wb")
    database  = open(dbpath)
    i         = -1
    parsing   = False
    
    for line in database:
        line = line.strip()
        if not parsing and line == "songList begin":
            parsing = True
        elif parsing and line == "songList end":
            parsing = False
            
        if parsing and not line == "songList begin":
            if line[0:5] == "key: ":
                i += 1
                tracks.append({"key" : "", "file" : "", "Time" : "", "Genre" : "", "Title" : "", "Artist" : "", "Date" : "", "Album" : "", "Track" : "", "mtime" : ""})
                
            splitted = line.split(": ", 1)
            tracks[i][splitted[0]] = splitted[1]
                    
    cPickle.dump(tracks, cachefile)
    database.close()
    cachefile.close()
else:
    print "Loading cache file."
    cachefile = open(cachepath, "rb")
    tracks = cPickle.load(cachefile)
    cachefile.close()
