#ifndef __KEYBOARD_H
#define __KEYBOARD_H 1

void keyboard_lights();
void toggleflag(u8int scancode);
void keyboard_handler(regs_t *r);
void setup_keyboard();

#endif
