#ifndef __PIT_H
#define __PIT_H

void timer_phase(int hz);
void timer_handler(struct regs *r);
void sleep(int ticks);
void timer_install();

#endif
