#ifndef __KERNEL_H
#define __KERNEL_H 1

/* definitions */
#define TRUE    1
#define FALSE   0
#define NULL    (char) 0
#define KSILLY  0
#define KDEBUG  1
#define KINFO   2
#define KWARN   3
#define KERROR  4
#define VERSION "0.0"
#define BUILD   "3"

/* typedefs */
typedef          int   size_t;
typedef unsigned long  u32int;
typedef          long  s32int;
typedef unsigned short u16int;
typedef          short s16int;
typedef unsigned char  u8int;
typedef          char  s8int;

/* structs */
typedef struct regs
{
    u32int gs, fs, es, ds;
    u32int edi, esi, ebp, esp, ebx, edx, ecx, eax;
    u32int int_no, err_code;
    u32int eip, cs, eflags, useresp, ss;    
} regs_t;

/* These are needed by *everything* */
#include <multiboot.h>
#include <string.h>

/* kernel.c functions */
u8int *memcpy(u8int *dest, const u8int *src, size_t count);
u8int *memset(u8int *dest, const u8int val, size_t count);
u16int *memsetw(u16int *dest, const u16int val, size_t count);
u32int *memsetdw(u32int *dest, const u32int val, size_t count);
u8int inportb(u16int _port);
u16int inportw(u16int _port);
u32int inportl(u16int _port);
void outportb(u16int _port, u8int _data);
void outportw(u16int _port, u16int _data);
void outportl(u16int _port, u32int _data);
void status(u8int* sender, u8int* message, u8int mode);
void panic(u8int* message);
void parse_command_line(u8int *cmdline);
void show_banner();
void mbinfodump(multiboot_info_t mbi);
void boot0(multiboot_info_t mbi, unsigned int magic);
void boot1(multiboot_info_t mbi, unsigned int magic);
void boot2(multiboot_info_t mbi, unsigned int magic);

#endif
