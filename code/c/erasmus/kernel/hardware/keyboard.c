#include <kernel.h>
#include <string.h>
#include <irqs.h>
#include <hardware/keyboard.h>
#include <hardware/vga.h>

kblayout_t layouts[1];
u32int numlayouts;
u32int curlayout;
u8int flags;
u8int locks;

void keyboard_lights()
{
    while((inportb(0x64) & 2)); /* wait 'til it's not busy */

    outportb(0xED, locks);
}

void toggleflag(u8int scancode)
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

void keyboard_handler(regs_t *r)
{
    u8int scancode = inportb(0x60);

    toggleflag(scancode);

    if(scancode & 0x80)
    {
	/* Key release */
	/* Nothing to see here... */
    } else {
	/* Key press */	
	u8int key = layouts[curlayout].lower[scancode];

	if(flags & 0x80) /* Shifted */
	{
	    /* Note: shift currently acts as a lock at the moment, which is irritating... */
	    if(scancode <= 55)
		key = layouts[curlayout].upper[scancode];
	} else if(locks & 0x20) {/* Caps lock */
	    /* Only shift *letters* in caps lock */
	    if((scancode >= 16 && scancode <= 25) || (scancode >= 30 && scancode <= 38) || (scancode >= 44 && scancode <= 50))
		key = layouts[curlayout].upper[scancode];
	}

	putch(key);
    }
}

void change_layout(u8int *layout)
{
    u32int i;
    u32int found = 0;
    for(i = 0; i < numlayouts && !found; i ++)
    {
	if(strcmp(layouts[i].name, layout))
	{
	    status((u8int*) "kb", ksprintf((u8int*) "Loading keyboard layout '%s'", layout), KINFO);
	    curlayout = i;
	    found = 1;
	}
    }

    if(!found)
    {
	status((u8int*) "kb", ksprintf((u8int*) "Keyboard layout '%s' not found. Using default layout '%s'", layout, layouts[0].name), KWARN);
	curlayout = 0;
    }
}

void setup_keyboard(u8int *layout)
{
    flags = 0; /* shift, control, alt */
    locks = 0; /* scroll, num, caps */

    layouts[0].name = (u8int*) "gb";
    numlayouts = 1;

    /* Todo: Think of a better way to load the keyboard layout(s). Perhaps from a file when the VFS/ext2 is done... */
    u8int gb[128] = {
	0,  27, '1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '-', '=', '\b', '\t', 'q', 'w', 'e', 'r', 't', 'y', 'u', 'i', 'o', 'p', '[', ']', '\n',
	0, 'a', 's', 'd', 'f', 'g', 'h', 'j', 'k', 'l', ';', '\'', '#', 0, '\\', 'z', 'x', 'c', 'v', 'b', 'n', 'm', ',', '.', '/', 0, '*', 0, ' ',
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, '-', 0, 0, 0, '+', 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    };
    u8int GB[128] = {
	0,  27, '!', '"', '3', '$', '%', '^', '&', '*', '(', ')', '_', '+', 0, 0, 'Q', 'W', 'E', 'R', 'T', 'Y', 'U', 'I', 'O', 'P', '{', '}', 0,
	0, 'A', 'S', 'D', 'F', 'G', 'H', 'J', 'K', 'L', ':', '@', '~', 0, '|', 'Z', 'X', 'C', 'V', 'B', 'N', 'M', '<', '>', '?', 0,
    };

    int i;
    for(i = 0; i < 128; i ++)
    {
	layouts[0].lower[i] = gb[i];
	layouts[0].upper[i] = GB[i];
    }

    irq_install_handler(1, keyboard_handler);

    keyboard_lights();
    
    status((u8int*) "kb", (u8int*) "Set up keyboard handler", KINFO);

    change_layout(layout);
}
