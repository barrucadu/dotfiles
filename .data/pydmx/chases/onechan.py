import threading, time

class chase ( threading.Thread ):
    dmxcmd    = None
    startchan = 0
    speed     = 255
    running   = True

    def dmx (self, channel, value=None):
        # Use dmxcmd(channel, value, None, False, True) to set, dmxcmd(channel, None, None, False, True) to get
        # Obviously this function isn't required, but I preferred to abstract away the dmxcmd function
        return self.dmxcmd (channel + self.startchan, value, None, False, True)

    def delay (self):
        # Another unrequired function. But I want run to be tidy.
        time.sleep ((255 - speed) / 255) # Max one second between iterations, I think that's reasonable...

    def run (self):
        # This is a very simple chase. It fades a channel up and down.
        inc = True
        while running:
            val = self.dmx (0)
            
            if val == 255 or not inc:
                val -= 1
                inc = False
            elif val == 0 or inc:
                val += 1
                inc = True

            self.dmx (0, val)
            
            self.delay ()
