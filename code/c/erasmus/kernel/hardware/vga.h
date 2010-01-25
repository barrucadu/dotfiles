#ifndef __VGA_H
#define __VGA_H

/* Colours */
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

/* Video mode setting */
#define M40x25 0
#define M40x50 1
#define M80x25 2
#define M90x30 3
#define M90x60 4

#define VGA_AC_INDEX        0x3C0
#define VGA_AC_WRITE        0x3C0
#define VGA_AC_READ         0x3C1
#define VGA_MISC_WRITE      0x3C2
#define VGA_SEQ_INDEX       0x3C4
#define VGA_SEQ_DATA        0x3C5
#define VGA_DAC_READ_INDEX  0x3C7
#define VGA_DAC_WRITE_INDEX 0x3C8
#define VGA_DAC_DATA        0x3C9
#define VGA_MISC_READ       0x3CC
#define VGA_GC_INDEX        0x3CE
#define VGA_GC_DATA         0x3CF
#define VGA_CRTC_INDEX      0x3D4
#define VGA_CRTC_DATA       0x3D5
#define VGA_INSTAT_READ     0x3DA
#define VGA_NUM_SEQ_REGS    5
#define VGA_NUM_CRTC_REGS   25
#define VGA_NUM_GC_REGS     9
#define VGA_NUM_AC_REGS     21
#define VGA_NUM_REGS        (1 + VGA_NUM_SEQ_REGS + VGA_NUM_CRTC_REGS + VGA_NUM_GC_REGS + VGA_NUM_AC_REGS)

/* Functions */
void setup_vga(u32int highres);
void write_regs(u8int *regs);
void write_font(u8int *buf, u16int font_height);
void set_plane(u16int p);
void vmemwr(u16int dst_off, u8int *src, u16int count);
u16int get_fb_seg(void);
void scrollup();
void move_csr();
void cls();
void putch(u8int c);
void puts(u8int *str);
void settextcolour(u8int foreground, u8int background);

#endif
