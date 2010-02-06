#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <gd.h>
#include <glib.h>
#include <gtk/gtk.h>
#include <math.h>
#include "fractal.h"

/* Image options */
int image_width  = 1000;
int image_height = 800;
char* image_file   = (char*) "fractal.png";

/* Mandelbrot Set options */
int power = 2;

/* Misc options */
float re_min = -2.0;
float re_max = 1.0;
float re_range, re_factor;
float im_min = (float)-1.2;
float im_max, im_range, im_factor;
char* re_mins     = NULL;
char* re_maxs     = NULL;
char* im_mins     = NULL;

int iterations    = 30;
int show_axes     = FALSE;
int show_colour   = FALSE;
int smooth_colour = FALSE;
int inv_colour    = FALSE;
int greyscale     = FALSE;
int bluescale     = FALSE;
int redscale      = FALSE;
int greenscale    = FALSE;

/* Julia Set options */
int julia     = FALSE;
cmplx jul     = {0.0, 0.0};
char* re_juls = NULL;
char* im_juls = NULL;

/* Glib option parser variables */
GOptionEntry help_all[] =
{
    { "filename",     'f', 0, G_OPTION_ARG_STRING, &image_file,   "Filename to save to",                       NULL },
    { "image-width",  'W', 0, G_OPTION_ARG_INT,    &image_width,  "Generated image width",                     NULL },
    { "image-height", 'H', 0, G_OPTION_ARG_INT,    &image_height, "Generated image height",                    NULL },
    { "iterations",   'n', 0, G_OPTION_ARG_INT,    &iterations,   "Number of iterations to run on each point", NULL },
    { "re-min",       'r', 0, G_OPTION_ARG_STRING, &re_mins,      "Start value on the real axis",              NULL },
    { "re-max",       'R', 0, G_OPTION_ARG_STRING, &re_maxs,      "End value on the real axis",                NULL },
    { "im-min",       'i', 0, G_OPTION_ARG_STRING, &im_mins,      "Start value on the imaginary axis",         NULL },
    { "show-axes",    'a', 0, G_OPTION_ARG_NONE,   &show_axes,    "Show real and imaginary axes",              NULL },
    { NULL }
};

GOptionEntry help_colour[] =
{
    { "show-colour",  'c',  0, G_OPTION_ARG_NONE,   &show_colour,   "Show colour for points not in the set",   NULL },
    { "inv-colour",   'C',  0, G_OPTION_ARG_NONE,   &inv_colour,    "Invert all colours",                      NULL },
    { "greyscale",    '\0', 0, G_OPTION_ARG_NONE,   &greyscale,     "Use only shades of grey",                 NULL },
    { "blues",        '\0', 0, G_OPTION_ARG_NONE,   &bluescale,     "Use shades of blue",                      NULL },
    { "reds",         '\0', 0, G_OPTION_ARG_NONE,   &redscale,      "Use shades of red",                       NULL },
    { "greens",       '\0', 0, G_OPTION_ARG_NONE,   &greenscale,    "Use shades of green",                     NULL },
    { "smooth",       's',  0, G_OPTION_ARG_NONE,   &smooth_colour, "Smooth colours",                          NULL },
    { NULL }
};

GOptionEntry help_mandelbrot[] =
{
    { "power",        'p', 0, G_OPTION_ARG_INT,    &power,        "The power to raise z to (default 2)",       NULL },
    { NULL }
};

GOptionEntry help_julia[] =
{
    { "julia",        'j', 0, G_OPTION_ARG_NONE,   &julia,        "Generate a Julia set",                      NULL },
    { "julia-re",     'A', 0, G_OPTION_ARG_STRING, &re_juls,      "Real part for the Julia parameter",         NULL },
    { "julia-im",     'B', 0, G_OPTION_ARG_STRING, &im_juls,      "Imaginary part for the Julia parameter",    NULL },
    { NULL }
};

/* GD Variables */
gdImagePtr im;

/* Program begin */
int iteration = 0;
float zmod    = 0.0;

void cmplx_mul(cmplx z1, cmplx z2, float *re, float *im)
{
    float tmp_re, tmp_im;
    
    tmp_re = z1[0] * z2[0] - z1[1] * z2[1];
    tmp_im = z1[0] * z2[1] + z1[1] * z2[0];
    
    *re = tmp_re;
    *im = tmp_im;
}

void cmplx_pow(cmplx z, int pow, float *re, float *im)
{
    cmplx tmp1, tmp2;

    tmp1[0] = z[0];
    tmp1[1] = z[1];

    for(; pow > 1; --pow)
    {
	cmplx_mul(tmp1, tmp1, &tmp2[0], &tmp2[1]);

	tmp1[0] = tmp2[0];
	tmp1[1] = tmp2[1];
    }

    *re = tmp1[0];
    *im = tmp1[1];
}

int in_mandy_set(cmplx c)
{
    int   i, inset;
    cmplx d;
    float re2, im2;

    d[0] = c[0];
    d[1] = c[1];

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

	cmplx_pow(d, power, &d[0], &d[1]);
	d[0] = d[0] + c[0];
	d[1] = d[1] + c[1];	
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

    d[0] = c[0];
    d[1] = c[1];

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
	
	cmplx_pow(d, 2, &d[0], &d[1]);
	d[0] = d[0] + jul[0];
	d[1] = d[1] + jul[1];	
    }

    iteration = i;
    zmod      = cmplx_mod(d);

    return inset;
}

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
	    
	    if(greyscale)
	    {
		out[0] = (int) (255 - 255 * intensity);
		out[1] = (int) (255 - 255 * intensity);
		out[2] = (int) (255 - 255 * intensity);
	    } else {
		if(redscale)   out[0] = (int) (255 * intensity);
		if(greenscale) out[1] = (int) (255 * intensity);
		if(bluescale)  out[2] = (int) (255 * intensity);
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

int main(int argc, char *argv[])
{
    cmplx  c = {0, 0};
    colour t = {0, 0, 0};
    coord  p = {0, 0};
    float  n;

    GOptionContext *context;
    GOptionGroup   *gcolour;
    GOptionGroup   *gmandelbrot;
    GOptionGroup   *gjulia;

    context     = g_option_context_new("- Mandelbrot and Julia set generator");
    gcolour     = g_option_group_new("colour", "Options to control output colour", "Options to control output colour", NULL, NULL);
    gmandelbrot = g_option_group_new("mandy",  "Options specific to the Mandelbrot Set generator", "Options specific to the Mandelbrot Set generator", NULL, NULL);
    gjulia      = g_option_group_new("julia",  "Options specific to the Julia Set generator", "Options specific to the Julia Set generator", NULL, NULL);

    g_option_group_add_entries(gcolour,     help_colour);
    g_option_group_add_entries(gmandelbrot, help_mandelbrot);
    g_option_group_add_entries(gjulia,      help_julia);

    g_option_context_add_main_entries(context, help_all, NULL);
    g_option_context_add_group(context, gcolour);
    g_option_context_add_group(context, gmandelbrot);
    g_option_context_add_group(context, gjulia);
    g_option_context_parse(context, &argc, &argv, NULL);

    if(julia)
    {
	jul[0] = 0;
	jul[1] = 0;

	if(re_juls) jul[0] = (float)atof(re_juls);
	if(im_juls) jul[1] = (float)atof(im_juls);
    }

    if(re_mins) re_min = (float)atof(re_mins);
    if(re_maxs) re_max = (float)atof(re_maxs);
    if(im_mins) im_min = (float)atof(im_mins);
    
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

    save_set();

    return 0;
}
