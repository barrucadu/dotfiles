#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <gd.h>
#include <glib.h>
#include <gtk/gtk.h>
#include <math.h>
#include <pthread.h>

#include "main.h"
#include "fractal.h"
#include "image.h"
#include "cmplx.h"

int in_mandy_set(cmplx c, int *iteration, float *zmod, fractal_options_t foptions)
{
    int   i, inset;
    cmplx d;
    float re2, im2;

    cmplx_add(c, foptions.misc.param, &d[0], &d[1]);

    inset = 1;
    for(i = 0; i < foptions.misc.iterations; ++i)
    {
	if(foptions.mandelbrot.conjugate) cmplx_conj(d, &d[0], &d[1]);

	re2 = d[0] * d[0];
	im2 = d[1] * d[1];

	if((re2 + im2) > 4)
	{
	    inset = 0;
	    break;
	}

	cmplx_pow(d, foptions.mandelbrot.power, &d[0], &d[1]);
	cmplx_add(d, c, &d[0], &d[1]);
    }

    *iteration = i;
    *zmod      = cmplx_mod(d);

    return inset;
}

int in_julia_set(cmplx c, int *iteration, float *zmod, fractal_options_t foptions)
{
    int   i, inset;
    cmplx d;
    float re2, im2;

    cmplx_add(c, foptions.misc.param, &d[0], &d[1]);

    inset = 1;
    for(i = 0; i < foptions.misc.iterations; ++i)
    {
	re2 = d[0] * d[0];
	im2 = d[1] * d[1];
	
	if((re2 + im2) > 4)
	{
	    inset = 0;
	    break;
	}
	
	cmplx_pow(d, 2, &d[0], &d[1]);
	cmplx_add(d, foptions.misc.param, &d[0], &d[1]);
    }

    *iteration = i;
    *zmod      = cmplx_mod(d);

    return inset;
}

void fractal_main_loop(fractal_options_t foptions, image_options_t ioptions)
{
    /*printf("Re: %f - %f, Im: %f - %f\n",
	   foptions.plot.re_min, foptions.plot.re_max,
	   foptions.plot.im_min, foptions.plot.im_max);*/

    cmplx  c = {0, 0};
    colour t = {0, 0, 0};
    coord  p = {0, 0};
    int    inset, iteration;
    float  zmod;

    for(p[0] = 0; p[0] < ioptions.image.width; ++p[0])
    {
	c[0] = (float) (foptions.plot.re_min + p[0] * foptions.plot.re_factor);

	for(p[1] = 0; p[1] < ioptions.image.height; ++p[1])
	{
	    c[1] = (float) (foptions.plot.im_max - p[1] * foptions.plot.im_factor);

	    inset = (foptions.julia.julia) ? in_julia_set(c, &iteration, &zmod, foptions) : in_mandy_set(c, &iteration, &zmod, foptions);

	    colourise(inset, iteration, foptions.misc.iterations, zmod, &t[0], &t[1], &t[2], ioptions);
	    put_pixel(p, t, ioptions);
	}
    }

    if(foptions.plot.show_axes)
    {
	t[0] = 100;
	t[1] = 100;
	t[2] = 100;
	
	p[0] = 0;
	p[1] = (int) (foptions.plot.im_max / foptions.plot.im_factor);
	for(; p[0] < ioptions.image.width; ++p[0]) put_pixel(p, t, ioptions);
	
	p[0] = (int) (- foptions.plot.re_min / foptions.plot.re_factor);
	p[1] = 0;
	for(; p[1] < ioptions.image.height; ++p[1]) put_pixel(p, t, ioptions);
    }
}
