#include <stdio.h>
#include <stdlib.h>
#include <gd.h>
#include "mandelbrot.h"

gdImagePtr im;

int in_set(cmplx c)
{
    int   i = 0;
    cmplx d;
    float re2, im2;

    d[0] = c[0];
    d[1] = c[1];

    for(; i < ITERATIONS; ++i)
    {
	re2 = d[0] * d[0];
	im2 = d[1] * d[1];

	if((re2 + im2) > 4) break;

	d[0] = re2 - im2 + c[0];
	d[1] = 2 * d[0] * d[1] + c[1];	
    }

    return i;
}

void put_pixel(coord p, colour c)
{
    gdImageSetPixel(im, p[0], p[1], gdTrueColor(c[0], c[1], c[2]));
}

void save_set()
{
    FILE * pngout = fopen(IMAGE_FILE, "wb");
    gdImagePngEx(im, pngout, 0);
    gdImageDestroy(im);
    fclose(pngout);
}

int main()
{
    cmplx  c = {0, 0};
    colour t = {0, 0, 0};
    coord  p = {0, 0};
    int    n;

    im = gdImageCreateTrueColor(IMAGE_WIDTH, IMAGE_HEIGHT);

    for(p[0] = 0; p[0] < IMAGE_WIDTH; ++p[0])
    {
	c[0] = (float) (RE_MIN + p[0] * RE_FACTOR);

	for(p[1] = 0; p[1] < IMAGE_HEIGHT; ++p[1])
	{
	    c[1] = (float) (IM_MAX - p[1] * IM_FACTOR);

	    n = in_set(c);
	    
	    if(n == ITERATIONS)
	    {
		t[0] = 0;
		t[1] = 0;
		t[2] = 0;
	    } else {
		t[0] = (int) (255 - 255 * (float)n / ITERATIONS);
		t[1] = (int) (255 - 255 * (float)n / ITERATIONS);
		t[2] = (int) (255 - 255 * (float)n / ITERATIONS);
	    }
	    
	    put_pixel(p, t);
	}
    }

    if(SHOW_AXES)
    {
	/* Axes */
	t[0] = 100;
	t[1] = 100;
	t[2] = 100;
	
	/* Real */
	p[0] = 0;
	p[1] = (int) (IM_MAX / IM_FACTOR);
	for(; p[0] < IMAGE_WIDTH; ++p[0]) put_pixel(p, t);
	
	/* Imaginary */
	p[0] = (int) (- RE_MIN / RE_FACTOR);
	p[1] = 0;
	for(; p[1] < IMAGE_HEIGHT; ++p[1]) put_pixel(p, t);
    }

    save_set();

    return 0;
}
