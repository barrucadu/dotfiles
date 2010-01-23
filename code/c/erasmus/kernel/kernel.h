#ifndef __KERNEL_H
#define __KERNEL_H 1

#define MAX_INT_LEN 32

/* typedefs */
typedef int size_t;

/* structs */
struct regs
{
    unsigned int gs, fs, es, ds;
    unsigned int edi, esi, ebp, esp, ebx, edx, ecx, eax;
    unsigned int int_no, err_code;
    unsigned int eip, cs, eflags, useresp, ss;    
};

/* kernel.c functions */
unsigned char *memcpy(unsigned char *dest, const unsigned char *src, size_t count);
unsigned char *memset(unsigned char *dest, const unsigned char val, size_t count);
unsigned short *memsetw(unsigned short *dest, const unsigned short val, size_t count);
size_t strlen(const char* str);
unsigned char* strrev(unsigned char* string);
unsigned char* itos(int number, unsigned int base);
unsigned char inportb(unsigned short _port);
void outportb(unsigned short _port, unsigned char _data);
void panic(unsigned char* message);

#endif
