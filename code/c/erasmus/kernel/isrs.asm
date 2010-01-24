    ;; 'Quiet' ISR (no automatic error code)
%macro ISRQ 1
global isr%1
isr%1:
    cli
    push byte 0
    push byte %1
    jmp isr_common_stub
%endmacro

    ;; 'Noisy' ISR (automatic error code)
%macro ISRN 1
global isr%1
isr%1:
    cli
    push byte %1
    jmp isr_common_stub
%endmacro

ISRQ 0				; Division by zero
ISRQ 1				; Debug
ISRQ 2				; Non Maskable Interrupt
ISRQ 3				; Breakpoint
ISRQ 4				; Into Detected Overflow
ISRQ 5				; Out of Bounds
ISRQ 6				; Invalid Opcode
ISRQ 7				; No Coprocessor
ISRN 8				; Double Fault
ISRQ 9				; Coprocessor Segment Overrun
ISRN 10				; Bad TSS
ISRN 11				; Segment Not Present
ISRN 12				; Stack Fault
ISRN 13				; General Protection Fault
ISRN 14				; Page Fault
ISRQ 15				; Unknown Interrupt
ISRQ 16				; Coprocessor Fault
ISRQ 17				; Alignment Check
ISRQ 18				; Machine Check
ISRQ 19				; Reserved
ISRQ 20				; Reserved
ISRQ 21				; Reserved
ISRQ 22				; Reserved
ISRQ 23				; Reserved
ISRQ 24				; Reserved
ISRQ 25				; Reserved
ISRQ 26				; Reserved
ISRQ 27				; Reserved
ISRQ 28				; Reserved
ISRQ 29				; Reserved
ISRQ 30				; Reserved
ISRQ 31				; Reserved

extern fault_handler

    ;;  Magic code: Save CPU state, call the fault handler, and restore the stack frame.
isr_common_stub:
    pusha
    push ds
    push es
    push fs
    push gs
    
    mov ax, 0x10
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov gs, ax
    mov eax, esp

    push eax
    mov eax, fault_handler
    call eax

    pop eax
    pop gs
    pop fs
    pop es
    pop ds
    popa

    add esp, 8

    iret
