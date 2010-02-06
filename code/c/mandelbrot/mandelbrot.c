#include <stdio.h>
#include <stdlib.h>
#include <gd.h>
#include "mandelbrot.h"

/* Image options */
float image_width  = 1000;
float image_height = 800;
char* image_file   = (char*) "mandelbrot2.png";

/* Real axis options */
float re_min = -2.0;
float re_max = 1.0;
float re_range, re_factor;

/* Imaginary axis options */
float im_min = (float)-1.2;
float im_max, im_range, im_factor;

/* Misc options */
int iterations  = 255;
int show_axes   = FALSE;
int show_colour = TRUE;

gdImagePtr im;

int in_set(cmplx c)
{
    int   i;
    cmplx d;
    float re2, im2;

    d[0] = c[0];
    d[1] = c[1];

    for(i = 0; i < iterations; ++i)
    {
	re2 = d[0] * d[0];
	im2 = d[1] * d[1];

	if((re2 + im2) > 4) break;

	d[1] = 2 * d[0] * d[1] + c[1];	
	d[0] = re2 - im2 + c[0];
    }

    return i;
}

void put_pixel(coord p, colour c)
{
    gdImageSetPixel(im, p[0], p[1], gdTrueColor(c[0], c[1], c[2]));
}

void save_set()
{
    FILE * pngout = fopen(image_file, "wb");
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

    re_range  = re_max - re_min;
    re_factor = re_range / (image_width - 1);
    im_max    = im_min + re_range * image_height / image_width;
    im_range  = im_max - im_min;
    im_factor = im_range / (image_height - 1);

    im = gdImageCreateTrueColor((int)image_width, (int)image_height);

    for(p[0] = 0; p[0] < (int)image_width; ++p[0])
    {
	c[0] = (float) (re_min + (float)p[0] * re_factor);

	for(p[1] = 0; p[1] < (int)image_height; ++p[1])
	{
	    c[1] = (float) (im_max - (float)p[1] * im_factor);

	    n = in_set(c);
	    
	    if(n == iterations)
	    {
		t[0] = 0;
		t[1] = 0;
		t[2] = 0;
	    } else {
		if(show_colour)
		{
		    t[0] = (int) (255 - 255.0 * (float)n / iterations);
		    t[1] = (int) (255 - 255.0 * (float)n / iterations);
		    t[2] = (int) (255 - 255.0 * (float)n / iterations);
		} else {
		    t[0] = 255;
		    t[1] = 255;
		    t[2] = 255;
		}
	    }
	    
	    put_pixel(p, t);
	}
    }

    if(show_axes)
    {
	t[0] = 100;
	t[1] = 100;
	t[2] = 100;
	
	p[0] = 0;
	p[1] = (int) (im_max / im_factor);
	for(; p[0] < image_width; ++p[0]) put_pixel(p, t);
	
	p[0] = (int) (- re_min / re_factor);
	p[1] = 0;
	for(; p[1] < image_height; ++p[1]) put_pixel(p, t);
    }

    save_set();

    return 0;
}
