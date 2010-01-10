CFLAGS=$(shell pkg-config --cflags glib-2.0) -Wall -W -c -O3 -ggdb -march=core2 -mtune=native -pipe
LDFLAGS=$(shell pkg-config --libs glib-2.0) -Wl,-O1 -Wl,--sort-common -Wl,--as-needed -Wl,--hash-style=gnu

all: rc.sysinit.o
	gcc $(LDFLAGS) *.o -o rc.sysinit

clean:
	rm -f rc.sysinit
	rm -f *.o
