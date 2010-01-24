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

void panic(u8int* message)
{
    /* Simple kernel panic function. Todo: coredump */
    cls();

    puts((u8int*) "Kernel panic: ");
    puts(message);
    puts((u8int*) ".\n Halting system...");

    int irq;
    for(irq = 0; irq < 16; irq ++)
	irq_uninstall_handler(irq);

    for(;;);
}

void kmain(multiboot_info_t* mbi, unsigned int magic)
{
    /* Basic stuff */
    gdt_install();
    idt_install();
    isrs_install();
    irq_install();
    timer_install();
    timer_phase(100); /* Set the PIT to 100Hz */

    __asm__ __volatile__ ("sti");

    /* Extra stuff */
    setup_vga();
    setup_keyboard();

    if (magic != MULTIBOOT_BOOTLOADER_MAGIC)
    {
	/* GRUB fail... */
	panic((u8int*) "Invalid multiboot magic number.");
    }

    /* Let's grab the memory map GRUB built */
    mm_grab_map(mbi);
    /* mm_dump_map() */

    

    for(;;); /* There has got to be a more CPU-efficient way of sitting around and doing nothing */
}
