CC      := gcc
WARN    := -Wall -Wextra -Wshadow -Wpointer-arith -Wcast-align -Wwrite-strings -Wmissing-declarations -Wredundant-decls -Wnested-externs -Winline -Wno-long-long -Winit-self -Wmissing-prototypes -Werror
CFLAGS  := -O3 $(shell pkg-config --cflags gtk+-2.0) $(WARN) -ggdb -c
LDFLAGS := -lgd -lm $(shell pkg-config --libs gtk+-2.0)

.PHONY: all clean

all: fractal

fractal: fractal.o cmplx.o image.o

clean:
	-@rm fractal *.o