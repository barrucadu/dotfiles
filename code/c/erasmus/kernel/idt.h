#ifndef __IDT_H
#define II_DT_H

extern void idt_load();

void idt_set_gate(unsigned char num, unsigned long base, unsigned short sel, unsigned char flags);
void idt_install();

#endif
