#include <kernel.h>
#include <hardware/pci.h>

/* Note: This doesn't work properly. */

u32int pcibuildaddr(u16int bus, u16int device, u8int function, u8int reg)
{
    u32int address = 0x00;
    u32int lbus = (u32int) bus;
    u32int ldev = (u32int) device;
    u32int lfun = (u32int) function;
    u32int lreg = (u32int) reg;

    address = 0x80000000 | (lbus << 16) | (ldev << 11) | (lfun << 8) | lreg;
    return address;
}

u8int pcireadu8int(u16int bus, u16int device, u8int function, u8int reg)
{
    u32int data = pcireadu32int(bus, device, function, reg);
    return (u8int) (data & 0x000000FF);
}

u16int pcireadu16int(u16int bus, u16int device, u8int function, u8int reg)
{
    u32int data = pcireadu32int(bus, device, function, reg);
    return (u16int) (data & 0x0000FFFF);
}

u32int pcireadu32int(u16int bus, u16int device, u8int function, u8int reg)
{
    u32int address = pcibuildaddr(bus, device, function, reg);
    outportl(0xCF8, address);
    return inportl(0xCFC);
}

u8int pcipolldevice(u32int bus, u32int device)
{
    return !(pcireadu16int(bus, device, 0, 0) == 0xFFFF);
}

u16int pcivendor(u32int bus, u32int device)
{
    return pcireadu16int(bus, device, 0, 0);
}

u16int pcidevice(u32int bus, u32int device)
{
    return pcireadu16int(bus, device, 0, 0x02);
}

u8int pciclass(u32int bus, u32int device)
{
    return pcireadu8int(bus, device, 0, 0x0B);
}

u8int pcisubclass(u32int bus, u32int device)
{
    return pcireadu8int(bus, device, 0, 0x0A);
}

u8int pcirevision(u32int bus, u32int device)
{
    return pcireadu8int(bus, device, 0, 0x08);
}
