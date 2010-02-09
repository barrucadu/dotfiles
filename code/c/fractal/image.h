#ifndef __IMAGE_H
#define __IMAGE_H

/* Prototypes */
void put_pixel(coord p, colour c);
void save_set(void);
void colourise(int inset, int iteration, float zmod, int *r, int *g, int *b);
void init_im(void);

#endif
