#include <kernel.h>
#include <idt.h>

struct idt_entry
{
    unsigned short base_lo;
    unsigned short sel;     /* Our kernel segment goes here! */
    unsigned char always0;  /* This will ALWAYS be set to 0! */
    unsigned char flags;
    unsigned short base_hi;
} __attribute__ ((packed));

struct idt_ptr
{
    unsigned short limit;
    unsigned int base;
} __attribute__ ((packed));

struct idt_entry idt[256];
struct idt_ptr idtp;

void idt_set_gate(unsigned char num, unsigned long base, unsigned short sel, unsigned char flags)
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
    idtp.limit = (sizeof (struct idt_entry) * 256) - 1;
    idtp.base  = (int) &idt;

    memset((unsigned char*) &idt, 0, sizeof(struct idt_entry) * 256);
    idt_load();
}
