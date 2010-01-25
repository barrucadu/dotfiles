#ifndef __PCI_H
#define __PCI_H 1

u32int pcibuildaddr(u16int bus, u16int device, u8int function, u8int reg);
u8int pcireadu8int(u16int bus, u16int device, u8int function, u8int reg);
u16int pcireadu16int(u16int bus, u16int device, u8int function, u8int reg);
u32int pcireadu32int(u16int bus, u16int device, u8int function, u8int reg);
u8int pcipolldevice(u32int bus, u32int device);
u16int pcivendor(u32int bus, u32int device);
u16int pcidevice(u32int bus, u32int device);
u8int pciclass(u32int bus, u32int device);
u8int pcisubclass(u32int bus, u32int device);
u8int pcirevision(u32int bus, u32int device);

#endif
