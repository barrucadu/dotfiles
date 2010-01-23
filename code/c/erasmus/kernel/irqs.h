#ifndef __IRQS_H
#define __IRQS_H

extern void irq0();
extern void irq1();
extern void irq2();
extern void irq3();
extern void irq4();
extern void irq5();
extern void irq6();
extern void irq7();
extern void irq8();
extern void irq9();
extern void irq10();
extern void irq11();
extern void irq12();
extern void irq13();
extern void irq14();
extern void irq15();

extern void idt_set_gate(unsigned char num, unsigned long base, unsigned short sel, unsigned char flags);

void irq_install_handler(int irq, void (*handler)(struct regs *r));
void irq_uninstall_handler(int irq);
void irq_remap();
void irq_install();
void irq_handler(struct regs *r);

#endif
