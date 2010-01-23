#ifndef __KEYBOARD_H
#define __KEYBOARD_H

void keyboard_lights();
void toggleflag(unsigned char scancode);
void keyboard_handler(struct regs *r);
void setup_keyboard();

#endif
