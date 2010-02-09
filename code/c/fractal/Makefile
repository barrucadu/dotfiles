CC      := gcc
CFLAGS  := -O3 $(shell pkg-config --cflags gtk+-2.0) -Wall -c -ggdb -Wextra
LDFLAGS := -lgd -lm $(shell pkg-config --libs gtk+-2.0)

.PHONY: all clean

all: fractal

fractal: fractal.o cmplx.o image.o

clean:
	-@rm fractal *.o