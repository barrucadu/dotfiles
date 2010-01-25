#include <kernel.h>
#include <mm.h>

mmap_t mmap;
u32int mem_start;

void mm_grab_map(multiboot_info_t mbi)
{
    /* Parse the grub memory map and load info from it */

    mmap.addr   = mbi.mmap_addr;
    mmap.length = mbi.mmap_length;

    multiboot_memory_map_t* map = (multiboot_memory_map_t*) mmap.addr;

    status((u8int*) "mm", (u8int*) "Retrieved GRUB memory map", KINFO);
    
    u32int i;

    for(i = 0; (u32int) map < mmap.addr + mmap.length && i < 256; i ++)
    {
	mmap.mblock[i].address = map->addr;
	mmap.mblock[i].length  = map->len;
	mmap.mblock[i].type    = map->type;

	status((u8int*) "mm", ksprintf((u8int*) "Region: start %s, length %s, type %s", itos(map->addr, 16), itos(map->len, 16), itos(map->type, 16)), KDEBUG);

	map = (multiboot_memory_map_t*) ((u32int) map + map->size + sizeof(u32int));
    }

    mmap.blocks = i;
}

void mm_dump_map()
{
    /* Print out our memory map */

    kprintf((u8int*) "Memory map:\n");

    u32int i;
    for(i = 0; i < mmap.blocks; i ++)
    {
	kprintf((u8int*)  "    Address range: %s -> %s (%s), type %s\n",
		itos(mmap.mblock[i].address, 16), itos(mmap.mblock[i].address + mmap.mblock[i].length, 16),
		itos(mmap.mblock[i].length, 16),  itos(mmap.mblock[i].type, 16));
    }
}

u8int *kmalloc(u32int size)
{
    u8int *memory = (u8int*) mem_start;
    mem_start += size;
    return memory;
}
