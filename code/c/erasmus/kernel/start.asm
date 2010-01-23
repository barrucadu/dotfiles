BITS 32
global loader
extern kmain

MODULEALIGN equ  1<<0	                   ; align loaded modules on page boundaries
    MEMINFO     equ  1<<1                  ; provide memory map
    FLAGS       equ  MODULEALIGN | MEMINFO ; this is the Multiboot 'flag' field
    MAGIC       equ    0x1BADB002	   ; 'magic number' lets bootloader find the header
    CHECKSUM    equ -(MAGIC + FLAGS)	   ; checksum required

section .text
    align 4
MultiBootHeader:
       dd MAGIC
       dd FLAGS
       dd CHECKSUM

STACKSIZE equ 0x4000 		; 16KB

loader:
    mov esp, stack+STACKSIZE	; Set up the stack
    push eax			; Push the multiboot magic number
    push ebx			; Push the multiboot info stack
    
    call kmain
    cli
    hlt

SECTION .bss
align 4
stack:	
    resb STACKSIZE
