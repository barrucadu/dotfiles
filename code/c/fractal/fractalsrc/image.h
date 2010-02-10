#ifndef __IMAGE_H
#define __IMAGE_H

/* Prototypes */
void       put_pixel(coord p, colour c, image_options_t ioptions);
void       save_set(image_options_t ioptions);
void       colourise(int inset, int iteration, int iterations, float zmod, int *r, int *g, int *b, image_options_t ioptions);
gdImagePtr init_gd(image_options_t ioptions);

#endif
