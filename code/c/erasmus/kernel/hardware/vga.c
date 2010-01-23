#include <kernel.h>
#include <hardware/vga.h>

unsigned short *textmemptr;
unsigned blank;
int attrib;
int csr_x;
int csr_y;

void setup_vga()
{
    textmemptr = (unsigned short *)0xB8000;

    settextcolour(COL_WHITE, COL_BLACK); /* White text on a black background */

    csr_x = 0;
    csr_y = 0;

    cls();
}

void scrollup()
{
    memcpy((unsigned char*) textmemptr, (unsigned char*) textmemptr + 80, 2 * 80); /* Copy 80 shorts (2 bytes) of memory to `textmemptr` from 1 line of text down */
    memsetw(textmemptr + (csr_y - 1) * 80, blank, 80);    /* Write `blank` 80 times to the last line on the screen. */
    csr_y --;
}

void move_csr()
{
    /* VGA magic. */
    unsigned pos;
    pos = csr_y * 80 + csr_x;

    outportb(0x3D4, 14);
    outportb(0x3D5, pos >> 8);
    outportb(0x3D4, 15);
    outportb(0x3D5, pos);
}

void cls()
{
    memsetw(textmemptr, blank, 80 * 25); /* Set every character on the screen to the blank character. */

    /* Move the cursor back to the start. */
    csr_x = 0;
    csr_y = 0;
    move_csr();
}

void putch(unsigned char c)
{
    /* Special cases: c = backspace, tab, \n. We're going to use the UNIX line-ending convention. */
    if(c == 0x08)
    {
	if(csr_x > 0)
	{
	    csr_x --;
	}

    } else if(c == 0x09) {
	csr_x = (csr_x + 8) & ~(8 - 1); /* ~ = bitwise complement */

    } else if(c == '\n') {
	csr_x = 0;
	csr_y ++;

    } else if(c >= ' ') {
	unsigned short *loc;
	loc = textmemptr + (csr_y * 80 + csr_x); /* Memory address of the current cursor location. */
	*loc = c | (attrib << 8);
	csr_x ++;
    }

    /* If we've gone off the right edge of the screen: */
    if(csr_x > 80)
    {
	/* Go back to the beginning and advance a line. */
	csr_x = 0;
	csr_y ++;
    }

    /* If we've gone off the bottom of the screen: */
    if(csr_y == 26)
    {
	/* Scroll up a line */
	scrollup();
    }

    move_csr();
}

void puts(unsigned char *str)
{
    int i;
    for(i = 0; i < strlen((char*) str); i ++)
    {
	putch(str[i]);
    }
}

void settextcolour(unsigned char foreground, unsigned char background)
{
    attrib = (background << 4) | (foreground & 0x0F); /* Upper four bits are the background, lower four are the foreground. */
    blank  = 0x20 | (attrib << 8); /* Update our blank character to use the new background. */
}
