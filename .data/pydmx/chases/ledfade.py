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
        time.sleep ((255 - self.speed) / 255) # Max one second between iterations, I think that's reasonable...

    def run (self):
        # This is designed for the LED lamp. Funky behaviour will ensue if used with something else.
        inc = [True, True, True]

        # Initial colour (0, 50, 100)
        self.dmx (0, 0)
        self.dmx (1, 0)
        self.dmx (2, 50)
        self.dmx (3, 100)
        
        while self.running:
            val = [self.dmx (1), self.dmx (2), self.dmx (3)]

            for i in range(0, 3):
                if val[i] == 255 or (not inc[i] and val[i] > 0):
                    val[i] -= 1
                    inc[i] = False
                elif val[i] == 0 or inc[i]:
                    val[i] += 1
                    inc[i] = True

            for i in range(0, 3):
                self.dmx (i + 1, val[i])
            
            self.delay ()
