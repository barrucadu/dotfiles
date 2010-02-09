#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <gd.h>
#include <glib.h>
#include <gtk/gtk.h>
#include <math.h>

#include "fractal.h"
#include "image.h"
#include "cmplx.h"

/* Image options */
int image_width  = 1000;
int image_height = 800;
char* image_file = (char*) "fractal.png";

/* Colour options */
int show_colour   = FALSE;
int smooth_colour = FALSE;
int inv_colour    = FALSE;
float bluescale   = 0;
float redscale    = 0;
float greenscale  = 0;

/* GD Variables */
gdImagePtr im;

/* Externs defined in fractal.c */
extern int iterations;
extern int iteration;
extern float zmod;

void put_pixel(coord p, colour c)
{
    if(inv_colour)
    {
	c[0] = 255 - c[0];
	c[1] = 255 - c[1];
	c[2] = 255 - c[2];
    }

    gdImageSetPixel(im, p[0], p[1], gdTrueColor(c[0], c[1], c[2]));
}

void save_set()
{
    FILE * pngout = fopen(image_file, "wb");
    gdImagePngEx(im, pngout, 0);
    gdImageDestroy(im);
    fclose(pngout);
}

void colourise(int inset, int iteration, float zmod, int *r, int *g, int *b)
{
    colour out = {0, 0, 0};
    float intensity;

    if(!inset)
    {
	if(show_colour)
	{
	    if(smooth_colour)
	    {
		intensity = 1 / (iteration - logn(log(zmod), 2));
	    } else {
		intensity = (float)iteration / iterations;
	    }
	    
	    if(!redscale && !greenscale && !bluescale)
	    {
		out[0] = (int) (255 - 255 * intensity);
		out[1] = (int) (255 - 255 * intensity);
		out[2] = (int) (255 - 255 * intensity);
	    } else {
		if(redscale)   out[0] = (int) (255 * intensity * redscale);
		if(greenscale) out[1] = (int) (255 * intensity * greenscale);
		if(bluescale)  out[2] = (int) (255 * intensity * bluescale);
	    }
	} else {
	    out[0] = 255;
	    out[1] = 255;
	    out[2] = 255;
	}
    }
    
    *r = out[0];
    *g = out[1];
    *b = out[2];
}

void init_im()
{
    im = gdImageCreateTrueColor(image_width, image_height);
}
