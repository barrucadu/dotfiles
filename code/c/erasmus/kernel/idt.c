#include <kernel.h>
#include <idt.h>

idt_entry_t idt[256];
idt_ptr_t idtp;

void idt_set_gate(u8int num, u32int base, u16int sel, u8int flags)
{
    /* Set an IDT gate. This is *far* simpler than the GDT stuff */

    idt[num].base_lo = (base & 0xFFFF);
    idt[num].base_hi = (base >> 16) & 0xFFFF;
    idt[num].sel = sel;
    idt[num].always0 = 0;
    idt[num].flags = flags;
}

void idt_install()
{
    /* Set up the IDT pointer and add some stuff to it */
    idtp.limit = (sizeof (idt_entry_t) * 256) - 1;
    idtp.base  = (s32int) &idt;

    memset((u8int*) &idt, 0, sizeof(idt_entry_t) * 256);
    idt_load();

    status((u8int*) "idt", (u8int*) "Loaded IDT", 0);
}
