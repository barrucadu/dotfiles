#include <kernel.h>
#include <gdt.h>

struct gdt_entry
{
    unsigned short limit_low;
    unsigned short base_low;
    unsigned char base_middle;
    unsigned char access;
    unsigned char granularity;
    unsigned char base_high;
} __attribute__((packed)); /* `packed` stops the compiler from trying to optimise this struct. */

struct gdt_ptr
{
    unsigned short limit;
    unsigned int base;
} __attribute__((packed));

struct gdt_entry gdt[3];
struct gdt_ptr gp;

void gdt_set_gate(int num, unsigned long base, unsigned long limit, unsigned char access, unsigned char gran)
{
    /* Magic function to add a GDT entry */
    gdt[num].base_low = (base & 0xFFFF);
    gdt[num].base_middle = (base >> 16) & 0xFF;
    gdt[num].base_high = (base >> 24) & 0xFF;

    gdt[num].limit_low = (limit & 0xFFFF);
    gdt[num].granularity = ((limit >> 16) & 0x0F);

    gdt[num].granularity |= (gran & 0xF0);
    gdt[num].access = access;
}

void gdt_install()
{
    /* Set up a few GDT entries and load the new GDT over the GRUB one */
    gp.limit = (sizeof(struct gdt_entry) * 3) - 1;
    gp.base  = (int) &gdt;

    /* Add the null gate (memory protection thingy) */
    gdt_set_gate(0, 0, 0, 0, 0);

    /* Add the code segment: base address 0, limit 4GB, 4KB granularity, 32-bit opcodes. */
    gdt_set_gate(1, 0, 0xFFFFFFFF, 0x9A, 0xCF);

    /* Add the data segment: exactly the same as the code segment. */
    gdt_set_gate(2, 0, 0xFFFFFFFF, 0x92, 0xCF);

    /* Load our new GDT. */
    gdt_flush();
}
