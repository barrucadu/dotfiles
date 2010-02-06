CC     := gcc
WARN   := -Wall -Wextra -Wshadow -Wpointer-arith -Wcast-align -Wwrite-strings -Wmissing-declarations -Wredundant-decls -Wnested-externs -Winline -Wno-long-long -Winit-self -Wmissing-prototypes -Wstrict-prototypes -Wconversion -Werror -pedantic
CFLAGS := -O2 -lgd $(WARN)

.PHONY: all clean

all: mandelbrot

clean:
	-@rm mandelbrot