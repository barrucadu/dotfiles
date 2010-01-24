#include <kernel.h>
#include <multiboot.h>
#include <string.h>
#include <gdt.h>
#include <idt.h>
#include <isrs.h>
#include <irqs.h>
#include <pit.h>
#include <mm.h>
#include <hardware/vga.h>
#include <hardware/keyboard.h>

u8int *memcpy(u8int *dest, const u8int *src, size_t count)
{
    /* A function to copy `count` bytes of data from `src` to `dest`, returning `dest`. */

    for(; count != 0; count --)
    {
	*dest++ = *src++;
    }
    
    return dest;
}

u8int *memset(u8int *dest, const u8int val, size_t count)
{
    /* A function to set `count` bytes of data in `dest` equal to `val`, returning `dest`. */

    for(; count != 0; count --)
    {
	*dest++ = val;
    }
    
    return dest;
}

u16int *memsetw(u16int *dest, const u16int val, size_t count)
{
    /* A function to set `count` words of data in `dest` equal to `val`, returning `dest`. 16-bit. */
    
    for(; count != 0; count --)
    {
	*dest++ = val;
    }
    
    return dest;
}

u32int *memsetdw(u32int *dest, const u32int val, size_t count)
{
    /* A function to set `count` doublewords of data in `dest` equal to `val`, returning `dest`. 32-bit. */
    
    for(; count != 0; count --)
    {
	*dest++ = val;
    }
    
    return dest;
}

u8int inportb(u16int _port)
{
    /* A magic function to read one byte of data from a device. */

    u8int rv;

    __asm__ __volatile__ ("inb %1, %0" : "=a" (rv) : "dN" (_port));

    return rv;
}

void outportb(u16int _port, u8int _data)
{
    /* A magic function to write one byte of data to a device. */

    __asm__ __volatile__ ("outb %1, %0" : : "dN" (_port), "a" (_data));
}

void status(u8int* sender, u8int* message, u8int mode)
{
    /* mode = 0: information
       mode = 1: debug
       mode = 2: warning
       mode = 3: error */

    /* Todo: verbosity control through kernel command line */

    switch(mode)
    {
    case 0:
	settextcolour(COL_LIGHT_BLUE, COL_BLACK);
	break;
    case 1:
	settextcolour(COL_LIGHT_GREEN, COL_BLACK);
	break;
    case 2:
	settextcolour(COL_LIGHT_MAGENTA, COL_BLACK);
	break;
    case 3:
	settextcolour(COL_LIGHT_RED, COL_BLACK);
	break;
    }

    puts(ksprintf((u8int*)  "[%s] ", sender));
    settextcolour(COL_WHITE, COL_BLACK);

    u32int pad = 8 - (strlen(sender) + 2);
    
    for(; pad > 0; pad --)
    {
	putch((u8int) ' ');
    }

    puts(ksprintf((u8int*) "%s\n", message));
}

void panic(u8int* message)
{
    /* Simple kernel panic function. Todo: coredump */
    status((u8int*) "kmain", (u8int*) "Panic", 3);

    puts(ksprintf((u8int*) "Kernel panic: %s.\nHalting system...", message));

    int irq;
    for(irq = 0; irq < 16; irq ++)
	irq_uninstall_handler(irq);

    for(;;);
}

void kmain(multiboot_info_t* mbi, unsigned int magic)
{
    /* Level 0 boot:
     *  Initialise the screen to print status messages */
    setup_vga();

    /* Level 1 boot:
     *  Start up basic stuff: GDT and IDT, ISR and IRQ handlers, PIT control, memory manager */
    status((u8int*) "kmain", (u8int*) "Entering level 1 boot", 1);
    gdt_install();
    idt_install();
    isrs_install();
    irq_install();
    timer_install();

    __asm__ __volatile__ ("sti");

    if (magic != MULTIBOOT_BOOTLOADER_MAGIC)
    {
	/* GRUB fail... */
	panic((u8int*) "Invalid multiboot magic number.");
    }

    /* Let's grab the memory map GRUB built */
    mm_grab_map(*mbi);
    /* mm_dump_map(); */

    /* Level 2 boot:
     *  Extra stuff which isn't as exciting as what has already happened :p */
    status((u8int*) "kmain", (u8int*) "Entering level 2 boot", 1);
    setup_keyboard((u8int*) "gb"); /* Todo: Allow specifying a keyboard layout on the command line */

    /* putch('0' / 0); */
    
    status((u8int*) "kmain", (u8int*) "Entering idle loop", 1);
    for(;;); /* There has got to be a more CPU-efficient way of sitting around and doing nothing */
}
