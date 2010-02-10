#ifndef __FRACTAL_H
#define __FRACTAL_H

int  in_mandy_set(cmplx c, int *iteration, float *zmod, fractal_options_t foptions);
int  in_julia_set(cmplx c, int *iteration, float *zmod, fractal_options_t foptions);
void fractal_main_loop(fractal_options_t foptions, image_options_t ioptions);

#endif
