#ifndef __FRACTAL_H
#define __FRACTAL_H

/* Macros */
#define logn(x,b) (log(x) / log(b))

/* Typedefs */
typedef float cmplx[2];
typedef int   coord[2];
typedef int   colour[3];

/* Prototypes */
int in_mandy_set(cmplx c);
int in_julia_set(cmplx c);
int main(int argc, char *argv[]);

#endif
