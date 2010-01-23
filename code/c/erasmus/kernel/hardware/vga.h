#ifndef __VGA_H
#define __VGA_H

#define COL_BLACK         0
#define COL_BLUE          1
#define COL_GREEN         2
#define COL_CYAN          3
#define COL_RED           4
#define COL_MAGENTA       5
#define COL_BROWN         6
#define COL_LIGHT_GREY    7
#define COL_DARK_GREY     8
#define COL_LIGHT_BLUE    9
#define COL_LIGHT_GREEN   10
#define COL_LIGHT_CYAN    11
#define COL_LIGHT_RED     12
#define COL_LIGHT_MAGENTA 13
#define COL_LIGHT_BROWN   14
#define COL_WHITE         15

void setup_vga();
void scrollup();
void move_csr();
void cls();
void putch(unsigned char c);
void puts(unsigned char *str);
void settextcolour(unsigned char foreground, unsigned char background);

#endif
