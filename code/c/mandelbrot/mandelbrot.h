#ifndef __MANDELBROT_H
#define __MANDELBROT_H

/* Definitions */
#define IMAGE_WIDTH  1000.0
#define IMAGE_HEIGHT 800.0
#define IMAGE_FILE   "mandelbrot.png"

#define RE_MIN      -2.0
#define RE_MAX       1.0
#define RE_RANGE     (RE_MAX - RE_MIN)
#define RE_FACTOR    (RE_RANGE / (IMAGE_WIDTH - 1))

#define IM_MIN      -1.2
#define IM_MAX       (IM_MIN + RE_RANGE * IMAGE_HEIGHT / IMAGE_WIDTH)
#define IM_RANGE     (IM_MAX - IM_MIN)
#define IM_FACTOR    (IM_RANGE / (IMAGE_HEIGHT - 1))

#define ITERATIONS   50
#define SHOW_AXES    0

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
