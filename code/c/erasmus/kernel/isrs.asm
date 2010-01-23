global isr0
global isr1
global isr2
global isr3
global isr4
global isr5
global isr6
global isr7
global isr8
global isr9
global isr10
global isr11
global isr12
global isr13
global isr14
global isr15
global isr16
global isr17
global isr18
global isr19
global isr20
global isr21
global isr22
global isr23
global isr24
global isr25
global isr26
global isr27
global isr28
global isr29
global isr30
global isr31

    ;; Division by zero
isr0:
    cli
    push byte 0
    push byte 0
    jmp isr_common_stub

    ;; Debug
isr1:
    cli
    push byte 0
    push byte 1
    jmp isr_common_stub

    ;; Non Maskable Interupt
isr2:
    cli
    push byte 0
    push byte 2
    jmp isr_common_stub

    ;; Breakpoint
isr3:
    cli
    push byte 0
    push byte 3
    jmp isr_common_stub

    ;; Into Detected Overflow
isr4:
    cli
    push byte 0
    push byte 4
    jmp isr_common_stub

    ;; Out of Bounds
isr5:
    cli
    push byte 0
    push byte 5
    jmp isr_common_stub

    ;; Invalid Opcode
isr6:
    cli
    push byte 0
    push byte 6
    jmp isr_common_stub

    ;; No Coprocessor
isr7:
    cli
    push byte 0
    push byte 7
    jmp isr_common_stub

    ;; Double Fault
isr8:
    cli
    push byte 8
    jmp isr_common_stub

    ;; Coprocessor Segment Overrun
isr9:
    cli
    push byte 0
    push byte 9
    jmp isr_common_stub

    ;; Bad TSS
isr10:
    cli
    push byte 10
    jmp isr_common_stub

    ;; Segment Not Present
isr11:
    cli
    push byte 11
    jmp isr_common_stub

    ;; Stack Fault
isr12:
    cli
    push byte 12
    jmp isr_common_stub

    ;; General Protection Fault
isr13:
    cli
    push byte 13
    jmp isr_common_stub

    ;; Page Fault
isr14:
    cli
    push byte 14
    jmp isr_common_stub

    ;; Unknown Interrupt
isr15:
    cli
    push byte 0
    push byte 15
    jmp isr_common_stub

    ;; Coprocessor Fault
isr16:
    cli
    push byte 0
    push byte 16
    jmp isr_common_stub

    ;; Alignment Check
isr17:
    cli
    push byte 0
    push byte 17
    jmp isr_common_stub

    ;; Machine Check
isr18:
    cli
    push byte 0
    push byte 18
    jmp isr_common_stub

    ;; Reserved
isr19:
    cli
    push byte 0
    push byte 19
    jmp isr_common_stub

    ;; Reserved
isr20:
    cli
    push byte 0
    push byte 20
    jmp isr_common_stub

    ;; Reserved
isr21:
    cli
    push byte 0
    push byte 21
    jmp isr_common_stub

    ;; Reserved
isr22:
    cli
    push byte 0
    push byte 22
    jmp isr_common_stub

    ;; Reserved
isr23:
    cli
    push byte 0
    push byte 23
    jmp isr_common_stub

    ;; Reserved
isr24:
    cli
    push byte 0
    push byte 24
    jmp isr_common_stub

    ;; Reserved
isr25:
    cli
    push byte 0
    push byte 25
    jmp isr_common_stub

    ;; Reserved
isr26:
    cli
    push byte 0
    push byte 26
    jmp isr_common_stub

    ;; Reserved
isr27:
    cli
    push byte 0
    push byte 27
    jmp isr_common_stub

    ;; Reserved
isr28:
    cli
    push byte 0
    push byte 28
    jmp isr_common_stub

    ;; Reserved
isr29:
    cli
    push byte 0
    push byte 29
    jmp isr_common_stub

    ;; Reserved
isr30:
    cli
    push byte 0
    push byte 30
    jmp isr_common_stub

    ;; Reserved
isr31:
    cli
    push byte 0
    push byte 31
    jmp isr_common_stub

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
