#ifndef __KEYBOARD_H
#define __KEYBOARD_H 1

/* Structs */

typedef struct kblayout
{
    u8int *name; /* Layout name */
    u8int lower[128]; /* Unshifted */
    u8int upper[128]; /* Shifted */
} kblayout_t;

/* Functions */
void keyboard_lights();
void toggleflag(u8int scancode);
void keyboard_handler(regs_t *r);
void change_layout(u8int *layout);
void setup_keyboard(u8int *layout);

#endif
