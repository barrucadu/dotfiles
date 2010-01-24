#include <kernel.h>
#include <multiboot.h>
#include <mm.h>
#include <string.h>
#include <hardware/vga.h>

mmap_t mmap;

void mm_grab_map(multiboot_info_t* mbi)
{
    /* Parse the grub memory map and load info from it */

    mmap.addr   = mbi->mmap_addr;
    mmap.length = mbi->mmap_length;

    multiboot_memory_map_t* map = (multiboot_memory_map_t*) mmap.addr;
    
    u32int i;

    for(i = 0; (u32int) map < mmap.addr + mmap.length && i < 256; i ++)
    {
	mmap.mblock[i].address = map->addr;
	mmap.mblock[i].length  = map->len;
	mmap.mblock[i].type    = map->type;

	map = (multiboot_memory_map_t*) ((u32int) map + map->size + sizeof(u32int));
    }

    mmap.blocks = i;
}

void mm_dump_map()
{
    /* Print out our memory map */

    puts((u8int*) "Memory map:\n");

    u32int i;
    for(i = 0; i < mmap.blocks; i ++)
    {
	puts((u8int*) "    Address range: ");
	puts(itos(mmap.mblock[i].address, 16));
	puts((u8int*) " -> ");
	puts(itos(mmap.mblock[i].address + mmap.mblock[i].length, 16));
	puts((u8int*) " (");
	puts(itos(mmap.mblock[i].length, 16));
	puts((u8int*) "), type ");
	puts(itos(mmap.mblock[i].type, 16));
	putch('\n');
    }
}
