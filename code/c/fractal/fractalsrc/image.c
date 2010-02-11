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

void put_pixel(coord p, colour c, image_options_t ioptions)
{
    if(ioptions.colour.inv_colour)
    {
	c[0] = 255 - c[0];
	c[1] = 255 - c[1];
	c[2] = 255 - c[2];
    }

    gdImageSetPixel(ioptions.gd.im, p[0], p[1], gdTrueColor(c[0], c[1], c[2]));
}

void merge_and_save_sets(thread_options_t *toptions, int threads, char* file)
{
    int swidth = toptions[0].ioptions.image.width;
    int width  = swidth * threads;
    int height = toptions[0].ioptions.image.height;

    gdImagePtr im;
    im = gdImageCreateTrueColor(width, height);

    int i;
    for(i = 0; i < threads; ++i)
    {
	gdImageCopy(im, toptions[i].ioptions.gd.im, swidth * i, 0, 0, 0, swidth, height);
	gdImageDestroy(toptions[i].ioptions.gd.im);
    }
    
    FILE * pngout = fopen(file, "wb");
    gdImagePngEx(im, pngout, 0);
    gdImageDestroy(im);
    fclose(pngout);
}

void colourise(int inset, int iteration, int iterations, float zmod, int *r, int *g, int *b, image_options_t ioptions)
{
    colour out = {0, 0, 0};
    float intensity;

    if(!inset)
    {
	if(ioptions.colour.show_colour)
	{
	    if(ioptions.colour.smooth_colour)
	    {
		intensity = 1 / ((float)iteration - logn(log(zmod), 2));
	    } else {
		intensity = ((float)iteration / (float)iterations);
	    }
	    
	    if(!ioptions.colour.redscale && !ioptions.colour.greenscale && !ioptions.colour.bluescale)
	    {
		out[0] = (int) (255 - 255 * intensity);
		out[1] = (int) (255 - 255 * intensity);
		out[2] = (int) (255 - 255 * intensity);
	    } else {
		if(ioptions.colour.redscale)   out[0] = (int) (255 * intensity * ioptions.colour.redscale);
		if(ioptions.colour.greenscale) out[1] = (int) (255 * intensity * ioptions.colour.greenscale);
		if(ioptions.colour.bluescale)  out[2] = (int) (255 * intensity * ioptions.colour.bluescale);
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

void init_gd(image_options_t *ioptions)
{
    ioptions->gd.im = gdImageCreateTrueColor(ioptions->image.width, ioptions->image.height);
}
