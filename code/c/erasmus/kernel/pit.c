#include <kernel.h>
#include <pit.h>
#include <irqs.h>

u32int curticks = 0;

void timer_phase(u32int hz)
{
    /* Magic function */

    u32int divisor = 1193180 / hz;       /* Calculate our divisor */
    outportb(0x43, 0x36);             /* Set our command byte 0x36 */
    outportb(0x40, divisor & 0xFF);   /* Set low byte of divisor */
    outportb(0x40, divisor >> 8);     /* Set high byte of divisor */
}

void timer_handler(regs_t *r)
{
    /* Not doing much currently... */
    curticks ++;
}

void sleep(u32int ticks)
{
    u32int end = curticks + ticks;

    while(curticks < end);
}

void timer_install()
{
    /* The PIT fires on IRQ 0 */
    irq_install_handler(0, timer_handler);
    timer_phase(100); /* Set the PIT to 100Hz */

    status((u8int*) "time", (u8int*) "Installed PIT handler", 0);
}
