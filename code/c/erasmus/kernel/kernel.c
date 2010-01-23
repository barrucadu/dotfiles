#include <kernel.h>
#include <multiboot.h>
#include <gdt.h>
#include <idt.h>
#include <isrs.h>
#include <irqs.h>
#include <pit.h>
#include <mm.h>
#include <hardware/vga.h>
#include <hardware/keyboard.h>

unsigned char *memcpy(unsigned char *dest, const unsigned char *src, size_t count)
{
    /* A function to copy `count` bytes of data from `src` to `dest`, returning `dest`. */

    for(; count != 0; count --)
    {
	*dest++ = *src++;
    }
    
    return dest;
}

unsigned char *memset(unsigned char *dest, const unsigned char val, size_t count)
{
    /* A function to set `count` bytes of data in `dest` equal to `val`, returning `dest`. */

    for(; count != 0; count --)
    {
	*dest++ = val;
    }
    
    return dest;
}

unsigned short *memsetw(unsigned short *dest, const unsigned short val, size_t count)
{
    /* A function to set `count` bytes of data in `dest` equal to `val`, returning `dest`. 16-bit. */
    
    for(; count != 0; count --)
    {
	*dest++ = val;
    }
    
    return dest;
}

size_t strlen(const char* str)
{
    /* A function to return the number of bytes in a string. */
    const char *p = str;

    while (*p != '\0')
    {
	p++;
    }

    return (size_t) (p - str);
}

unsigned char* strrev(unsigned char* string)
{
    unsigned char* str;
    int i, len;

    len = strlen((const char*) string);

    for(i = 0; i < len; i ++)
    {
	str[i] = string[len - i - 1];
    }
    str[i] = '\0';
    
    return str;
}

unsigned char* itos(int number, unsigned int base)
{
    /* Todo: Allow choice between bases 2, 8, 10, and 16 (maybe also 1 for the lulz) */
    /* Note: Doesn't seem to work properly (eg, 57 returns "55"), and needs extending to work on long longs */

    /* Slightly modified example from K&R book */
    int i, sign;
    unsigned char *str;

    if ((sign = number) < 0)
        number = -number;

    i = 0;
    do  /* generate digits in reverse order */
    {
        str[i++] = (number % 10) + '0';   /* get next digit */
    } while (number /= 10);

    if (sign < 0)
        str[i++] = '-';
    str[i] = '\0';

    return strrev(str);
}

unsigned char inportb(unsigned short _port)
{
    /* A magic function to read one byte of data from a device. */

    unsigned char rv;

    __asm__ __volatile__ ("inb %1, %0" : "=a" (rv) : "dN" (_port));

    return rv;
}

void outportb(unsigned short _port, unsigned char _data)
{
    /* A magic function to write one byte of data to a device. */

    __asm__ __volatile__ ("outb %1, %0" : : "dN" (_port), "a" (_data));
}

void panic(unsigned char* message)
{
    /* Simple kernel panic function. Todo: coredump */
    cls();

    puts((unsigned char*) "Kernel panic: ");
    puts(message);
    puts((unsigned char*) ".\n Halting system...");

    int irq;
    for(irq = 0; irq < 16; irq ++)
	irq_uninstall_handler(irq);

    for(;;);
}

void kmain(multiboot_info_t* mbi, unsigned int magic)
{
    gdt_install();
    idt_install();
    isrs_install();
    irq_install();
    timer_install();

    __asm__ __volatile__ ("sti");
    
    setup_vga();
    setup_keyboard();

    if (magic != MULTIBOOT_BOOTLOADER_MAGIC)
    {
	/* GRUB fail... */
	panic((unsigned char*) "Invalid multiboot magic number.");
    }

    mm_grab_map(mbi);

    unsigned char *cmdline;

    cmdline = (unsigned char *) mbi->cmdline;

    puts((unsigned char*) "Kernel command line: ");
    puts(cmdline);

    puts(itos(57, 10));

    puts((unsigned char*) "\n\n");
    mm_dump_map();

    for(;;); /* There has got to be a more CPU-efficient way of sitting around and doing nothing */
}
