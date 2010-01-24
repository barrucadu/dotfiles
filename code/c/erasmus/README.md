Erasmus
-------

Erasmus is a simple operating system written in C and x86 assembly. The (eventual) goal is for a 64-bit (near) POSIX compliant self-hosted OS.

Version Plan
============

 * 0.1 plan:
   * Working memory manager (paging)
   * ATA driver (grab info about HDDs, able to read/write sectors)
 * 0.2 plan:
   * UNIX-like VFS (/dev, /proc, /root, etc)
   * Ext2 and FAT32 drivers
   * Basic libc
   * ELF loader
   * Multithreading
   * Multitasking
 * 0.3 plan
   * status() output to file
   * 1024x768 framebuffer
   * More advanced libc
   * Port of Aprz shell

Kernel Command Line
===================

Erasmus supports the passing of options via GRUB's kernel command line in a "key=value" format. In the case that the same key is specified twice, the last provided value is used. Note that, currently, all keys and values are case sensitive.

### out

Used to control the verbosity of Erasmus during boot. Accepted values:

 * out=kerror - only print error messages
 * out=kwarn  - print warnings.
 * out=kinfo  - print informational messages.
 * out=kdebug - print debugging information.
 * out=ksilly - print everything.

Each level also includes the output of the levels preceeding it. If unspecified, the default verbosity is kinfo.

### keymap

Used to control the keyboard map loaded in the final stage (level 2) of the booting process. Accepted values:

 * keymap=gb

If unspecified, the default keymap is gb.

### mode

Used to control the screen resolution in text mode. Accepted values:

 * mode=40x25
 * mode=40x50
 * mode=80x25
 * mode=90x30
 * mode=90x60

If unspecified, the default mode is 80x25.
