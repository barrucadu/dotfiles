#include <kernel.h>
#include <pit.h>
#include <irqs.h>

int curticks = 0;

void timer_phase(int hz)
{
    /* Magic function */

    int divisor = 1193180 / hz;       /* Calculate our divisor */
    outportb(0x43, 0x36);             /* Set our command byte 0x36 */
    outportb(0x40, divisor & 0xFF);   /* Set low byte of divisor */
    outportb(0x40, divisor >> 8);     /* Set high byte of divisor */
}

void timer_handler(struct regs *r)
{
    /* Not doing much currently... */
    curticks ++;
}

void sleep(int ticks)
{
    int end = curticks + ticks;

    while(curticks < end);
}

void timer_install()
{
    /* The PIT fires on IRQ 0 */
    irq_install_handler(0, timer_handler);
}
