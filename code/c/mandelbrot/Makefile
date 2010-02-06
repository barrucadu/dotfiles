CC      := gcc
CFLAGS  := -O3 $(shell pkg-config --cflags gtk+-2.0) -Wall
LDFLAGS := -lgd $(shell pkg-config --libs gtk+-2.0)

.PHONY: all clean

all: mandelbrot

clean:
	-@rm mandelbrot