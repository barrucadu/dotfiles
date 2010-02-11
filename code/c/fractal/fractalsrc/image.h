#ifndef __IMAGE_H
#define __IMAGE_H

/* Prototypes */
void put_pixel(coord p, colour c, image_options_t ioptions);
void merge_and_save_sets(thread_options_t *toptions, int threads, char* file);
void colourise(int inset, int iteration, int iterations, float zmod, int *r, int *g, int *b, image_options_t ioptions);
void init_gd(image_options_t *ioptions);

#endif
