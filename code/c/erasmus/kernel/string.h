#ifndef __STRING_H
#define __STRING_H 1

/* GCC magic */
#define va_start(v,l) __builtin_va_start(v,l)
#define va_arg(v,l)   __builtin_va_arg(v,l)
#define va_end(v)     __builtin_va_end(v)
#define va_copy(d,s)  __builtin_va_copy(d,s)
typedef __builtin_va_list va_list;

/* Functions */
size_t strlen(const u8int* str);
u8int* strrev(u8int* string);
u8int* ksprintf(u8int* format, ...);
u8int* itos(u32int number, u32int base);

#endif
