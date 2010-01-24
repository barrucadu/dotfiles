#include <kernel.h>
#include <hardware/vga.h>
#include <hardware/vga-modes.h>

/* This file contains a lot of magic. Fix that. */
#define	_vmemwr(DS,DO,S,N)	memcpy((char *)((DS) * 16 + (DO)), S, N)

u16int *textmemptr;
u32int blank;
u32int attrib;
u32int csr_x, csr_y;
u32int rows, cols;

void setup_vga(u32int highres)
{
    if(highres)
    {
	/* Let's try and set up 90x60 text mode :D */
	cols = 90;
	rows = 60;

	write_regs(g_90x60_text);
	write_font(g_8x8_font, 8);
    } else {
	cols = 80;
	rows = 25;

	write_regs(g_80x25_text);
	write_font(g_8x16_font, 16);
    }
    
    textmemptr = (u16int *)0xB8000;

    settextcolour(COL_WHITE, COL_BLACK); /* White text on a black background */

    csr_x = 0;
    csr_y = 0;

    cls();

    status((u8int*) "vga", (u8int*) "Initialised VGA", KINFO);
}

/* Start magic */
void write_regs(unsigned char *regs)
{
    unsigned i;

/* write MISCELLANEOUS reg */
    outportb(VGA_MISC_WRITE, *regs);
    regs++;
/* write SEQUENCER regs */
    for(i = 0; i < VGA_NUM_SEQ_REGS; i++)
    {
	outportb(VGA_SEQ_INDEX, i);
	outportb(VGA_SEQ_DATA, *regs);
	regs++;
    }
/* unlock CRTC registers */
    outportb(VGA_CRTC_INDEX, 0x03);
    outportb(VGA_CRTC_DATA, inportb(VGA_CRTC_DATA) | 0x80);
    outportb(VGA_CRTC_INDEX, 0x11);
    outportb(VGA_CRTC_DATA, inportb(VGA_CRTC_DATA) & ~0x80);
/* make sure they remain unlocked */
    regs[0x03] |= 0x80;
    regs[0x11] &= ~0x80;
/* write CRTC regs */
    for(i = 0; i < VGA_NUM_CRTC_REGS; i++)
    {
	outportb(VGA_CRTC_INDEX, i);
	outportb(VGA_CRTC_DATA, *regs);
	regs++;
    }
/* write GRAPHICS CONTROLLER regs */
    for(i = 0; i < VGA_NUM_GC_REGS; i++)
    {
	outportb(VGA_GC_INDEX, i);
	outportb(VGA_GC_DATA, *regs);
	regs++;
    }
/* write ATTRIBUTE CONTROLLER regs */
    for(i = 0; i < VGA_NUM_AC_REGS; i++)
    {
	(void)inportb(VGA_INSTAT_READ);
	outportb(VGA_AC_INDEX, i);
	outportb(VGA_AC_WRITE, *regs);
	regs++;
    }
/* lock 16-color palette and unblank display */
    (void)inportb(VGA_INSTAT_READ);
    outportb(VGA_AC_INDEX, 0x20);
}

void write_font(unsigned char *buf, unsigned font_height)
{
	unsigned char seq2, seq4, gc4, gc5, gc6;
	unsigned i;

/* save registers
set_plane() modifies GC 4 and SEQ 2, so save them as well */
	outportb(VGA_SEQ_INDEX, 2);
	seq2 = inportb(VGA_SEQ_DATA);

	outportb(VGA_SEQ_INDEX, 4);
	seq4 = inportb(VGA_SEQ_DATA);
/* turn off even-odd addressing (set flat addressing)
assume: chain-4 addressing already off */
	outportb(VGA_SEQ_DATA, seq4 | 0x04);

	outportb(VGA_GC_INDEX, 4);
	gc4 = inportb(VGA_GC_DATA);

	outportb(VGA_GC_INDEX, 5);
	gc5 = inportb(VGA_GC_DATA);
/* turn off even-odd addressing */
	outportb(VGA_GC_DATA, gc5 & ~0x10);

	outportb(VGA_GC_INDEX, 6);
	gc6 = inportb(VGA_GC_DATA);
/* turn off even-odd addressing */
	outportb(VGA_GC_DATA, gc6 & ~0x02);
/* write font to plane P4 */
	set_plane(2);
/* write font 0 */
	for(i = 0; i < 256; i++)
	{
	    vmemwr(16384u * 0 + i * 32, buf, font_height);
	    buf += font_height;
	}
/* restore registers */
	outportb(VGA_SEQ_INDEX, 2);
	outportb(VGA_SEQ_DATA, seq2);
	outportb(VGA_SEQ_INDEX, 4);
	outportb(VGA_SEQ_DATA, seq4);
	outportb(VGA_GC_INDEX, 4);
	outportb(VGA_GC_DATA, gc4);
	outportb(VGA_GC_INDEX, 5);
	outportb(VGA_GC_DATA, gc5);
	outportb(VGA_GC_INDEX, 6);
	outportb(VGA_GC_DATA, gc6);
}

void set_plane(unsigned p)
{
	unsigned char pmask;

	p &= 3;
	pmask = 1 << p;
/* set read plane */
	outportb(VGA_GC_INDEX, 4);
	outportb(VGA_GC_DATA, p);
/* set write plane */
	outportb(VGA_SEQ_INDEX, 2);
	outportb(VGA_SEQ_DATA, pmask);
}

void vmemwr(unsigned dst_off, unsigned char *src, unsigned count)
{
	_vmemwr(get_fb_seg(), dst_off, src, count);
}

unsigned get_fb_seg(void)
{
	unsigned seg;

	outportb(VGA_GC_INDEX, 6);
	seg = inportb(VGA_GC_DATA);
	seg >>= 2;
	seg &= 3;
	switch(seg)
	{
	case 0:
	case 1:
		seg = 0xA000;
		break;
	case 2:
		seg = 0xB000;
		break;
	case 3:
		seg = 0xB800;
		break;
	}
	return seg;
}
/* End magic */

void scrollup()
{
    u8int temp = csr_y - rows + 1;

    memcpy((u8int*) textmemptr, (u8int*) (textmemptr + temp * cols), (rows - temp) * cols * 2);
    memsetw(textmemptr + (rows - temp) * cols, blank, cols);

    csr_y = rows - 1;
}

void move_csr()
{
    /* VGA magic. */
    u32int pos;
    pos = csr_y * cols + csr_x;

    outportb(0x3D4, 14);
    outportb(0x3D5, pos >> 8);
    outportb(0x3D4, 15);
    outportb(0x3D5, pos);
}

void cls()
{
    memsetw(textmemptr, blank, rows * cols); /* Set every character on the screen to the blank character. */

    /* Move the cursor back to the start. */
    csr_x = 0;
    csr_y = 0;
    move_csr();
}

void putch(u8int c)
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
	u16int *loc;
	loc = textmemptr + (csr_y * cols + csr_x); /* Memory address of the current cursor location. */
	*loc = c | (attrib << 8);
	csr_x ++;
    }

    /* If we've gone off the right edge of the screen: */
    if(csr_x >= cols)
    {
	/* Go back to the beginning and advance a line. */
	csr_x = 0;
	csr_y ++;
    }

    /* If we've gone off the bottom of the screen: */
    while(csr_y >= rows)
    {
	/* Scroll up a line */
	scrollup();
    }

    move_csr();
}

void puts(u8int *str)
{
    u32int i;
    for(i = 0; str[i]; i ++)
    {
	putch(str[i]);
    }
}

void settextcolour(u8int foreground, u8int background)
{
    attrib = (background << 4) | (foreground & 0x0F); /* Upper four bits are the background, lower four are the foreground. */
    blank  = 0x20 | (attrib << 8); /* Update our blank character to use the new background. */
}
