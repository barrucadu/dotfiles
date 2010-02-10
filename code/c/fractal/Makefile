CC      := gcc
WARN    := -Wall -Wextra -Wshadow -Wpointer-arith -Wcast-align -Wwrite-strings -Wmissing-declarations -Wredundant-decls -Wnested-externs -Winline -Wno-long-long -Winit-self -Wmissing-prototypes -Werror
CFLAGS  := -O3 $(shell pkg-config --cflags gtk+-2.0) $(WARN) -ggdb -c
LDFLAGS := -lgd -lm $(shell pkg-config --libs gtk+-2.0)

.PHONY: all clean fractal fviewer

all: fractal fviewer

fractal: fractalsrc/main.o fractalsrc/fractal.o fractalsrc/cmplx.o fractalsrc/image.o
	@gcc $(LDFLAGS) -o fractal fractalsrc/*.o

fviewer: fviewersrc/fviewer.o
	@gcc $(LDFLAGS) -o fviewer fviewersrc/*.o

clean:
	-@rm fractal fviewer fractalsrc/*.o fviewersrc/*.o