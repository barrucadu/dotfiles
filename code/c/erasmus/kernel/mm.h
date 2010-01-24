#ifndef __MM_H
#define __MM_H 1

/* Structs */
typedef struct mblock
{
    u32int address;
    u32int length;
    u32int type;
} mblock_t;

typedef struct mmap
{
    u32int addr, length;
    mblock_t mblock[256];
    u32int blocks;
} mmap_t;

/* Functions */
void mm_grab_map(multiboot_info_t);
void mm_dump_map();
u8int *kmalloc(u32int size);

#endif
