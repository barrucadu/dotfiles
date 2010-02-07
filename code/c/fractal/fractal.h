#ifndef __FRACTAL_H
#define __FRACTAL_H

/* Macros */
#define logn(x,b)    (log(x) / log(b))
#define cmplx_mod(z) (sqrt(z[0] * z[0] + z[1] * z[1]))

/* Typedefs */
typedef float cmplx[2];
typedef int   coord[2];
typedef int   colour[3];

/* Prototypes */
void cmplx_mul(cmplx z1, cmplx z2, float *re, float *im);
void cmplx_pow(cmplx z, int pow, float *re, float *im);
void cmplx_conj(cmplx z, float *re, float *im);
int in_mandy_set(cmplx c);
int in_julia_set(cmplx c);
void put_pixel(coord p, colour c);
void save_set(void);
void colourise(int inset, int iteration, float zmod, int *r, int *g, int *b);
int main(int argc, char *argv[]);

#endif
