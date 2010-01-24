#ifndef __PIT_H
#define __PIT_H 1

void timer_phase(u32int hz);
void timer_handler(struct regs *r);
void sleep(u32int ticks);
void timer_install();

#endif
