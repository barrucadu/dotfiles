CFLAGS=-Wall -W -c -O3 -march=core2 -mtune=native -pipe
LDFLAGS=-Wl,-O1 -Wl,--sort-common -Wl,--as-needed -Wl,--hash-style=gnu

all: rc.sysinit.o rc.srvinit.o functions.o
	gcc $(LDFLAGS) rc.sysinit.o functions.o -o sysinitc
	gcc $(LDFLAGS) rc.srvinit.o functions.o -o srvinitc
	strip *initc

install: sysinitc srvinitc
	cp *initc /sbin/

clean:
	rm -f *initc
	rm -f *.o
