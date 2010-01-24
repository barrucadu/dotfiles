#include <kernel.h>
#include <gdt.h>

gdt_entry_t gdt[3];
gdt_ptr_t gp;

void gdt_set_gate(s32int num, u32int base, u32int limit, u8int access, u8int gran)
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
    gp.limit = (sizeof(gdt_entry_t) * 3) - 1;
    gp.base  = (s32int) &gdt;

    /* Add the null gate (memory protection thingy) */
    gdt_set_gate(0, 0, 0, 0, 0);

    /* Add the code segment: base address 0, limit 4GB, 4KB granularity, 32-bit opcodes. */
    gdt_set_gate(1, 0, 0xFFFFFFFF, 0x9A, 0xCF);

    /* Add the data segment: exactly the same as the code segment. */
    gdt_set_gate(2, 0, 0xFFFFFFFF, 0x92, 0xCF);

    /* Load our new GDT. */
    gdt_flush();
}
