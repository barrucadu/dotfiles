#include <kernel.h>
#include <gdt.h>
#include <idt.h>
#include <isrs.h>
#include <irqs.h>
#include <pit.h>
#include <mm.h>
#include <hardware/ide.h>
#include <hardware/pci.h>
#include <hardware/ata.h>
#include <hardware/vga.h>
#include <hardware/keyboard.h>

u32int verbosity = KINFO;
u32int vidmode   = M80x25;
u8int* keymap    = (u8int*) "gb";

extern u8int* res;

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
    u8int rv;

    __asm__ __volatile__ ("inb %1, %0" : "=a" (rv) : "dN" (_port));

    return rv;
}

u16int inportw(u16int _port)
{
    u16int rv;

    __asm__ __volatile__ ("inw %1, %0" : "=a" (rv) : "dN" (_port));

    return rv;
}

u32int inportl(u16int _port)
{
    u32int rv;

    __asm__ __volatile__ ("inl %1, %0" : "=a" (rv) : "dN" (_port));

    return rv;
}

void outportb(u16int _port, u8int _data)
{
    __asm__ __volatile__ ("outb %1, %0" : : "dN" (_port), "a" (_data));
}

void outportw(u16int _port, u16int _data)
{
    __asm__ __volatile__ ("outw %1, %0" : : "dN" (_port), "a" (_data));
}

void outportl(u16int _port, u32int _data)
{
    __asm__ __volatile__ ("outl %1, %0" : : "dN" (_port), "a" (_data));
}

void status(u8int* sender, u8int* message, u8int mode)
{
    if(mode >= verbosity)
    {
	switch(mode)
	{
	case KSILLY:
	    settextcolour(COL_LIGHT_CYAN, COL_BLACK);
	    break;
	case KDEBUG:
	    settextcolour(COL_LIGHT_GREEN, COL_BLACK);
	    break;
	case KINFO:
	    settextcolour(COL_LIGHT_BLUE, COL_BLACK);
	    break;
	case KWARN:
	    settextcolour(COL_LIGHT_MAGENTA, COL_BLACK);
	    break;
	case KERROR:
	    settextcolour(COL_LIGHT_RED, COL_BLACK);
	    break;
	}
	
	kprintf((u8int*)  " [%s] ", sender);
	settextcolour(COL_WHITE, COL_BLACK);
	
	u32int pad = 8 - (strlen(sender) + 2);
	
	for(; pad > 0; pad --)
	{
	    putch((u8int) ' ');
	}
	
	kprintf((u8int*) "%s\n", message);
    }
}

void panic(u8int* message)
{
    /* Simple kernel panic function. Todo: coredump */
    status((u8int*) "kmain", (u8int*) "Panic", KERROR);

    kprintf((u8int*) "Kernel panic: %s.\nHalting system...", message);

    int irq;
    for(irq = 0; irq < 16; irq ++)
	irq_uninstall_handler(irq);

    for(;;);
}

void parse_command_line(u8int* cmdline)
{
    /* Parse the cmdline into the options array */
    /* Bug: The first option found is messed up when printed... */
    u32int start = 0;
    u32int mid;
    u32int end;
    u8int *key, *var;

    while((start = strfindnext(cmdline, ' ', start))) /* First ' ' indicates end of kernel path */
    {
	mid = strfindnext(cmdline, '=', start);
	end = strfindnext(cmdline, ' ', mid); /* Now start-mid should be the key and mid-end the value */

	if(end == 0)
	{
	    end = strlen(cmdline);
	}

	key = substr(cmdline, start + 1, mid - start - 1);
	var = substr(cmdline, mid + 1, end - mid - 1);
	
	if(strcmp(key, (u8int*) "out")) /* Verbosity control */
	{
	    if(strcmp(var, (u8int*) "ksilly"))
	    {
		verbosity = KSILLY;
	    } else if(strcmp(var, (u8int*) "kdebug")) {
		verbosity = KDEBUG;
	    } else if(strcmp(var, (u8int*) "kinfo")) {
		verbosity = KINFO;
	    } else if(strcmp(var, (u8int*) "kwarn")) {
		verbosity = KWARN;
	    } else if(strcmp(var, (u8int*) "kerror")) {
		verbosity = KERROR;
	    }
	} else if(strcmp(key, (u8int*) "keymap")) { /* Keymap */
	    keymap = (u8int*) var;
	} else if(strcmp(key, (u8int*) "mode")) { /* Screen resolution */
	    if(strcmp(var, (u8int*) "40x25"))
	    {
		vidmode = M40x25;
	    } else if(strcmp(var, (u8int*) "40x50")) {
		vidmode = M40x50;
	    } else if(strcmp(var, (u8int*) "80x25")) {
		vidmode = M80x25;
	    } else if(strcmp(var, (u8int*) "90x30")) {
		vidmode = M90x30;
	    } else if(strcmp(var, (u8int*) "90x60")) {
		vidmode = M90x60;
	    }
	}
    }
}

void show_banner()
{
    cls();
#ifdef BUILD
    kprintf((u8int*) "\n ERASMUS %s-%s\n\n", VERSION, BUILD);
#else
    kprintf((u8int*) "\n ERASMUS %s\n\n", VERSION);
#endif
}

void mbinfodump(multiboot_info_t mbi)
{
    if(mbi.flags & MULTIBOOT_INFO_BOOT_LOADER_NAME)
    {
	status((u8int*) "kmain", ksprintf((u8int*) "Bootloader name '%s'", mbi.boot_loader_name), KDEBUG);
    }
}

void boot0(multiboot_info_t mbi, unsigned int magic)
{
    /* Level 0 boot:
     *  Initialise the screen to print status messages, check GRUB, and parse the command line */

    if (magic != MULTIBOOT_BOOTLOADER_MAGIC)
    {
	/* GRUB fail... */
	setup_vga(FALSE);
	panic((u8int*) "Invalid multiboot magic number.");
    }

    if(mbi.flags & MULTIBOOT_INFO_CMDLINE)
    {
	parse_command_line((u8int*) mbi.cmdline);
    }

    setup_vga(vidmode);
    show_banner();

    status((u8int*) "vga", ksprintf((u8int*) "Initialised screen to text mode %s", res), KINFO);

    mbinfodump(mbi);
}

void boot1(multiboot_info_t mbi, unsigned int magic)
{
    /* Level 1 boot:
     *  Start up basic stuff: GDT and IDT, ISR and IRQ handlers, PIT control, memory manager */
    
    (void) mbi;
    (void) magic;

    gdt_install();
    idt_install();
    isrs_install();
    irq_install();
    timer_install();

    __asm__ __volatile__ ("sti");

    /* Let's grab the memory map GRUB built */
    mm_grab_map(mbi);
    /* mm_dump_map(); */
}

void boot2(multiboot_info_t mbi, unsigned int magic)
{
    /* Level 2 boot:
     *  Extra stuff which isn't as exciting as what has already happened :p */

    (void) mbi;
    (void) magic;

    setup_keyboard(keymap);
    setup_ide();
    setup_ata();
}

void kmain(multiboot_info_t* mbi, unsigned int magic)
{
    boot0(*mbi, magic);

    status((u8int*) "kmain", (u8int*) "Entering level 1 boot", KINFO);
    boot1(*mbi, magic);

    status((u8int*) "kmain", (u8int*) "Entering level 2 boot", KINFO);
    boot2(*mbi, magic);

    /* PCI probe: for debugging / testing */
    u32int bus, device;

    /*pcivendor(0, 4);
      pcidevice(0, 4);*/

    for(bus = 0; bus < 256; bus ++)
    {
	for(device = 0; device < 256; device ++)
	{
	    if(pcipolldevice(bus, device))
	    {
		status((u8int*) "kmain",
		       ksprintf((u8int*) "Found PCI device at %s:%s (%s:%s, %s:%s, rev %s)",
				itos(bus, 10), itos(device, 10),
				itos(pcivendor(bus, device), 16),
				itos(pcidevice(bus, device), 16),
				itos(pciclass(bus, device), 16),
				itos(pcisubclass(bus, device), 16),
				itos(pcirevision(bus, device), 10)),
		       KDEBUG);
	    }
	}
    }

    /*putch('0' / 0);*/
    
    kprintf((u8int*) "\n ERASMUS %s-%s\n\n", VERSION, BUILD);
    status((u8int*) "kmain", (u8int*) "Boot complete. Entering idle loop", KINFO);
    for(;;); /* There has got to be a more CPU-efficient way of sitting around and doing nothing */
}
