#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <gd.h>
#include <glib.h>
#include <gtk/gtk.h>
#include <math.h>

#include "main.h"
#include "fractal.h"
#include "image.h"
#include "cmplx.h"

/* Externs defined in image.c */
extern int   image_width;
extern int   image_height;
extern char* image_file;
extern int   show_colour;
extern int   smooth_colour;
extern int   inv_colour;
extern float bluescale;
extern float redscale;
extern float greenscale;

/* Mandelbrot Set options */
int power     = 2;
int conjugate = FALSE;

/* Misc options */
float re_min = -2.0;
float re_max = 1.0;
float re_range, re_factor;

float im_min = (float)-1.2;
float im_max, im_range, im_factor;

cmplx param    = {0.0, 0.0};
int iterations = 30;
int show_axes  = FALSE;

/* Julia Set options */
int julia = FALSE;

/* Program begin */
int iteration = 0;
float zmod    = 0.0;

int in_mandy_set(cmplx c)
{
    int   i, inset;
    cmplx d;
    float re2, im2;

    cmplx_add(c, param, &d[0], &d[1]);

    inset = 1;
    for(i = 0; i < iterations; ++i)
    {
	if(conjugate) cmplx_conj(d, &d[0], &d[1]);

	re2 = d[0] * d[0];
	im2 = d[1] * d[1];

	if((re2 + im2) > 4)
	{
	    inset = 0;
	    break;
	}

	cmplx_pow(d, power, &d[0], &d[1]);
	cmplx_add(d, c, &d[0], &d[1]);
    }

    iteration = i;
    zmod      = cmplx_mod(d);

    return inset;
}

int in_julia_set(cmplx c)
{
    int   i, inset;
    cmplx d;
    float re2, im2;

    cmplx_add(c, param, &d[0], &d[1]);

    inset = 1;
    for(i = 0; i < iterations; ++i)
    {
	re2 = d[0] * d[0];
	im2 = d[1] * d[1];
	
	if((re2 + im2) > 4)
	{
	    inset = 0;
	    break;
	}
	
	cmplx_pow(d, 2,     &d[0], &d[1]);
	cmplx_add(d, param, &d[0], &d[1]);
    }

    iteration = i;
    zmod      = cmplx_mod(d);

    return inset;
}

void fractal_main_loop()
{
    cmplx  c = {0, 0};
    colour t = {0, 0, 0};
    coord  p = {0, 0};
    int    n;

    for(p[0] = 0; p[0] < image_width; ++p[0])
    {
	c[0] = (float) (re_min + p[0] * re_factor);

	for(p[1] = 0; p[1] < image_height; ++p[1])
	{
	    c[1] = (float) (im_max - p[1] * im_factor);

	    n = 0;
	    if(julia)
	    {
		n = in_julia_set(c);
	    } else {
		n = in_mandy_set(c);
	    }

	    colourise(n, iteration, zmod, &t[0], &t[1], &t[2]);
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
}
