CFLAGS=-Wall -W -c -O3 -march=core2 -mtune=native -pipe
LDFLAGS=-Wl,-O1 -Wl,--sort-common -Wl,--as-needed -Wl,--hash-style=gnu

all: rc.sysinit.o functions.o
	gcc $(LDFLAGS) *.o -o rc.sysinit
	strip rc.sysinit

install: rc.sysinit
	cp rc.sysinit /sbin/sysinitc

clean:
	rm -f rc.sysinit
	rm -f *.o
