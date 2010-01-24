#include <kernel.h>
#include <irqs.h>

void *irq_routines[16] = {
    0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0
};

/* Add and remove function pointers to the irq_routines array. */
void irq_install_handler(s32int irq, void (*handler)(regs_t *r))
{
    irq_routines[irq] = handler;
}

void irq_uninstall_handler(s32int irq)
{
    irq_routines[irq] = 0;
}


void irq_remap()
{
    /* We now send commands to the PICs to remap IRQs 0-7 to IDT entries 32-47. Magic */
    outportb(0x20, 0x11);
    outportb(0xA0, 0x11);
    outportb(0x21, 0x20);
    outportb(0xA1, 0x28);
    outportb(0x21, 0x04);
    outportb(0xA1, 0x02);
    outportb(0x21, 0x01);
    outportb(0xA1, 0x01);
    outportb(0x21, 0x0);
    outportb(0xA1, 0x0);
}

void irq_install()
{
    irq_remap();

    idt_set_gate(32, (unsigned)irq0, 0x08, 0x8E);
    idt_set_gate(33, (unsigned)irq1, 0x08, 0x8E);
    idt_set_gate(34, (unsigned)irq2, 0x08, 0x8E);
    idt_set_gate(35, (unsigned)irq3, 0x08, 0x8E);
    idt_set_gate(36, (unsigned)irq4, 0x08, 0x8E);
    idt_set_gate(37, (unsigned)irq5, 0x08, 0x8E);
    idt_set_gate(38, (unsigned)irq6, 0x08, 0x8E);
    idt_set_gate(39, (unsigned)irq7, 0x08, 0x8E);
    idt_set_gate(40, (unsigned)irq8, 0x08, 0x8E);
    idt_set_gate(41, (unsigned)irq9, 0x08, 0x8E);
    idt_set_gate(42, (unsigned)irq10, 0x08, 0x8E);
    idt_set_gate(43, (unsigned)irq11, 0x08, 0x8E);
    idt_set_gate(44, (unsigned)irq12, 0x08, 0x8E);
    idt_set_gate(45, (unsigned)irq13, 0x08, 0x8E);
    idt_set_gate(46, (unsigned)irq14, 0x08, 0x8E);
    idt_set_gate(47, (unsigned)irq15, 0x08, 0x8E);
}

void irq_handler(regs_t *r)
{
    /* Now we call handler functions for the IRQs. */

    void (*handler)(regs_t *r);
    handler = irq_routines[r->int_no - 32];

    if(handler)
    {
	handler(r);
    }

    /* If IRQ 8 -15, we need to tell the slave PIC. */
    if (r->int_no >= 40)
    {
        outportb(0xA0, 0x20);
    }
    /* ...and the master PIC, in both cases. */
    outportb(0x20, 0x20);
}
