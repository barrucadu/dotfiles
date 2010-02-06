#ifndef __MANDELBROT_H
#define __MANDELBROT_H

/* Definitions */
#define TRUE  1
#define FALSE 0

/* Typedefs */
typedef float cmplx[2];
typedef int   coord[2];
typedef int   colour[3];

/* Prototypes */
int in_set(cmplx c);
void put_pixel(coord p, colour c);
void save_set(void);
int main(void);

#endif
