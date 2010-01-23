#include <kernel.h>
#include <hardware/keyboard.h>
#include <hardware/vga.h>
#include <irqs.h>

/* Lower-case layout */
unsigned char kbdgb[128] =
{
    0,  27, '1', '2', '3', '4', '5', '6', '7', '8',
    '9', '0', '-', '=', '\b',
    '\t',
    'q', 'w', 'e', 'r',
    't', 'y', 'u', 'i', 'o', 'p', '[', ']', '\n',
    0, /* Control */
    'a', 's', 'd', 'f', 'g', 'h', 'j', 'k', 'l', ';',
    '\'', '#',   0, /* Left shift */
    '\\', 'z', 'x', 'c', 'v', 'b', 'n',/* 49 */
    'm', ',', '.', '/',   0, /* Right shift */
    '*',
    0, /* Alt */
    ' ',
    0, /* Caps lock */
    0, /* 59 - F1 key ... > */
    0,   0,   0,   0,   0,   0,   0,   0,
    0, /* < ... F10 */
    0, /* 69 - Num lock*/
    0, /* Scroll Lock */
    0, /* Home key */
    0, /* Up Arrow */
    0, /* Page Up */
    '-',
    0, /* Left Arrow */
    0,
    0, /* Right Arrow */
    '+',
    0, /* 79 - End key*/
    0, /* Down Arrow */
    0, /* Page Down */
    0, /* Insert Key */
    0, /* Delete Key */
    0,   0,   0,
    0, /* F11 Key */
    0, /* F12 Key */
    0, /* All other keys are undefined */
};

/* Upper-case/shifted layout */
unsigned char kbdGB[128] =
{
    0,  27, '!', '"', '3', '$', '%', '^', '&', '*', '(', ')', '_', '+', 0, 0, 'Q', 'W', 'E', 'R', 'T', 'Y', 'U', 'I', 'O', 'P', '{', '}', 0,
    0, 'A', 'S', 'D', 'F', 'G', 'H', 'J', 'K', 'L', ':', '@', '~', 0, '|', 'Z', 'X', 'C', 'V', 'B', 'N', 'M', '<', '>', '?', 0,
};

unsigned char flags;
unsigned char locks;

void keyboard_lights()
{
    while((inportb(0x64) & 2)); /* wait 'til it's not busy */

    outportb(0xED, locks);
}

void toggleflag(unsigned char scancode)
{
    int lights = 0;

    switch(scancode)
    {
	/* Flags: */
    case 42:
    case 54:
	/* Shift */
	flags = flags ^ 0x80;
	break;
	    
    case 29:
	/* Control */
	flags = flags ^ 0x40;
	break;
	
    case 56:
	/* Alt */
	flags = flags ^ 0x20;
	break;
	
	/* Locks: */
    case 70:
	/* Scroll */
	locks = locks ^ 0x80;
	lights = 1;
	break;

    case 69:
	/* Num */
	locks = locks ^ 0x40;
	lights = 1;
	break;
	
    case 58:
	/* Caps */
	locks = locks ^ 0x20;
	lights = 1;
	break;
    }

    if(lights)
	keyboard_lights();
}

void keyboard_handler(struct regs *r)
{
    unsigned char scancode = inportb(0x60);

    toggleflag(scancode);

    if(scancode & 0x80)
    {
	/* Key release */
	/* Nothing to see here... */
    } else {
	/* Key press */	
	unsigned char key = kbdgb[scancode];

	if(flags & 0x80) /* Shifted */
	{
	    /* Note: shift currently acts as a lock at the moment, which is irritating... */
	    if(scancode <= 55)
		key = kbdGB[scancode];
	} else if(locks & 0x20) {/* Caps lock */
	    /* Only shift *letters* in caps lock */
	    if((scancode >= 16 && scancode <= 25) || (scancode >= 30 && scancode <= 38) || (scancode >= 44 && scancode <= 50))
		key = kbdGB[scancode];
	}

	putch(key);
    }
}

void setup_keyboard()
{
    flags = 0; /* shift, control, alt */
    locks = 0; /* scroll, num, caps */

    irq_install_handler(1, keyboard_handler);

    keyboard_lights();
}
