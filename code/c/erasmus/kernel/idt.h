#ifndef __IDT_H
#define __IDT_H 1

/* Structs */
typedef struct idt_entry
{
    u16int base_lo;
    u16int sel;     /* Our kernel segment goes here! */
    u8int always0;  /* This will ALWAYS be set to 0! */
    u8int flags;
    u16int base_hi;
} __attribute__ ((packed)) idt_entry_t;

typedef struct idt_ptr
{
    u16int limit;
    u32int base;
} __attribute__ ((packed)) idt_ptr_t;

/* Functions */
extern void idt_load();

void idt_set_gate(u8int num, u32int base, u16int sel, u8int flags);
void idt_install();

#endif
