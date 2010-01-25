CC := gcc

WARN   := -Wall -Wextra
CFLAGS := -nostdlib -nostartfiles -nodefaultlibs -fno-builtin -O -m32 -I./kernel/ -c $(WARN)

LDFLAGS := -T kernel/link.ld -melf_i386

ASMSOURCES  := kernel/gdt.asm kernel/idt.asm kernel/irqs.asm kernel/isrs.asm
BASESOURCES := kernel/kernel.c kernel/string.c kernel/gdt.c kernel/idt.c kernel/isrs.c kernel/irqs.c kernel/pit.c kernel/mm.c
HWSOURCES   := kernel/hardware/vga.c kernel/hardware/keyboard.c kernel/hardware/ata.c kernel/hardware/pci.c kernel/hardware/ide.c

.PHONY: all kernel kasm kbase khardware libc qemu clean

all: kernel

install: erasmus.gz
	@cp erasmus.gz /boot/

uninstall: /boot/erasmus.gz
	@rm /boot/erasmus.gz

kernel: kasm kbase khardware
	ld $(LDFLAGS) -o erasmus kernel/start kernel/*.oasm kernel/*.o kernel/hardware/*.o
	gzip -f erasmus

kasm:      kernel/start         $(ASMSOURCES:.asm=.oasm)
kbase:     $(BASESOURCES:.c=.o) $(BASESOURCES:.c=.h)
khardware: $(HWSOURCES:.c=.o)   $(HWSOURCES:.c=.h)       kernel/hardware/vga-modes.h

kernel/start: kernel/start.asm
	nasm -f elf -o kernel/start kernel/start.asm

# Todo: tidy up this *.oasm chunk somehow - condense into 1 rule if possible
kernel/gdt.oasm: kernel/gdt.asm
	nasm -f elf -o kernel/gdt.oasm kernel/gdt.asm

kernel/idt.oasm: kernel/idt.asm
	nasm -f elf -o kernel/idt.oasm kernel/idt.asm

kernel/irqs.oasm: kernel/irqs.asm
	nasm -f elf -o kernel/irqs.oasm kernel/irqs.asm

kernel/isrs.oasm: kernel/isrs.asm
	nasm -f elf -o kernel/isrs.oasm kernel/isrs.asm

qemu: erasmus.gz image/erasmus.img
	@mount -o loop,offset=32256 -t ext2 image/erasmus.img image/mnt

	@mkdir -p image/mnt/boot/
	@mkdir -p image/mnt/lib/

	@cp erasmus.gz image/mnt/boot/

	@umount image/mnt

clean:
	-@rm kernel/start kernel/*.oasm kernel/*.o kernel/hardware/*.o
