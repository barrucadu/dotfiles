#include <kernel.h>
#include <multiboot.h>
#include <mm.h>
#include <hardware/vga.h>

struct mblock_t
{
    unsigned long long address;
    unsigned long long length;
    unsigned int type;
};

struct mmap_t
{
    unsigned int addr, length;
    struct mblock_t mblock[256];
    unsigned int blocks;
} mmap;

void mm_grab_map(multiboot_info_t* mbi)
{
    /* Parse the grub memory map and load info from it */

    mmap.addr   = mbi->mmap_addr;
    mmap.length = mbi->mmap_length;

    multiboot_memory_map_t* map = (multiboot_memory_map_t*) mmap.addr;
    
    int i;

    for(i = 0; (unsigned int) map < mmap.addr + mmap.length && i < 256; i ++)
    {
	mmap.mblock[i].address = map->addr;
	mmap.mblock[i].length  = map->len;
	mmap.mblock[i].type    = map->type;

	map = (multiboot_memory_map_t*) ((unsigned int) map + map->size + sizeof(unsigned int));
    }

    mmap.blocks = i;
}

void mm_dump_map()
{
    /* Print out our memory map */

    puts((unsigned char*) "Memory map start: ");
    puts(itos(mmap.addr, 16));
    puts((unsigned char*) ", length: ");
    puts(itos(mmap.length, 16));
    putch('\n');

    unsigned int i;
    for(i = 0; i < mmap.blocks; i ++)
    {
	puts((unsigned char*) "    Address range: ");
	puts(itos(mmap.mblock[i].address, 16));
	puts((unsigned char*) " -> ");
	puts(itos(mmap.mblock[i].address + mmap.mblock[i].length, 16));
	puts((unsigned char*) " (");
	puts(itos(mmap.mblock[i].length, 16));
	puts((unsigned char*) "), type ");
	puts(itos(mmap.mblock[i].type, 16));
	putch('\n');
    }
}
