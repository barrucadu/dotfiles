CC := gcc

WARN   := -Wall -Wextra
CFLAGS := -nostdlib -nostartfiles -nodefaultlibs -fno-builtin -O -m32 -I./kernel/ -c $(WARN)

LDFLAGS := -T kernel/link.ld -melf_i386

.PHONY: all kernel kasm kbase khardware libc qemu clean

all: kernel libc

kernel: kasm kbase khardware
	@ld $(LDFLAGS) -o Kernel kernel/start kernel/*.oasm kernel/*.o kernel/hardware/*.o
	@gzip -f Kernel

kasm: kernel/start.asm kernel/gdt.asm kernel/idt.asm kernel/irqs.asm kernel/isrs.asm
	@nasm -f elf -o kernel/start     kernel/start.asm
	@nasm -f elf -o kernel/gdt.oasm  kernel/gdt.asm
	@nasm -f elf -o kernel/idt.oasm  kernel/idt.asm
	@nasm -f elf -o kernel/irqs.oasm kernel/irqs.asm
	@nasm -f elf -o kernel/isrs.oasm kernel/isrs.asm

kbase: kernel/kernel.o kernel/string.o kernel/gdt.o kernel/idt.o kernel/isrs.o kernel/irqs.o kernel/pit.o kernel/mm.o

khardware: kernel/hardware/vga.o kernel/hardware/keyboard.o

libc:
	@echo "No POSIX-compliant libc implementation yet."

qemu: Kernel.gz image/erasmus.img
	@mount -o loop,offset=32256 -t ext2 image/erasmus.img image/mnt

	@mkdir -p image/mnt/boot/
	@mkdir -p image/mnt/lib/

	@cp Kernel.gz image/mnt/boot/

	@umount image/mnt

clean:
	-@rm kernel/start kernel/*.oasm kernel/*.o kernel/hardware/*.o
