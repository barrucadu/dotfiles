#ifndef __GDT_H
#define __GDT_H 1

/* Structs */
typedef struct gdt_entry
{
    u16int limit_low;
    u16int base_low;
    u8int base_middle;
    u8int access;
    u8int granularity;
    u8int base_high;
} __attribute__((packed)) gdt_entry_t; /* `packed` stops the compiler from trying to optimise this struct. */

typedef struct gdt_ptr
{
    u16int limit;
    u32int base;
} __attribute__((packed)) gdt_ptr_t;

/* Functions */
extern void gdt_flush();

void gdt_set_gate(s32int num, u32int base, u32int limit, u8int access, u8int gran);
void gdt_install();

#endif
