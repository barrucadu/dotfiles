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

fractal_options_t foptions;
image_options_t   ioptions;
float_options_t   floptions;
int threads = 1; /* This is *only* used in the main function, and it doesn't really fit anywhere else */

/* Glib option parser variables */
GOptionEntry help_all[] =
{
    { "filename",     'f',  0, G_OPTION_ARG_STRING, &ioptions.image.file,         "Filename to save to",                       NULL },
    { "image-width",  'W',  0, G_OPTION_ARG_INT,    &ioptions.image.width,        "Generated image width",                     NULL },
    { "image-height", 'H',  0, G_OPTION_ARG_INT,    &ioptions.image.height,       "Generated image height",                    NULL },
    { "iterations",   'n',  0, G_OPTION_ARG_INT,    &foptions.misc.iterations,    "Number of iterations to run on each point", NULL },
    { "re-min",       'r',  0, G_OPTION_ARG_STRING, &floptions.re_mins,           "Start value on the real axis",              NULL },
    { "re-max",       'R',  0, G_OPTION_ARG_STRING, &floptions.re_maxs,           "End value on the real axis",                NULL },
    { "im-min",       'i',  0, G_OPTION_ARG_STRING, &floptions.im_mins,           "Start value on the imaginary axis",         NULL },
    { "re",           'a',  0, G_OPTION_ARG_STRING, &floptions.param_re,          "Real part for the parameter",               NULL },
    { "im",           'b',  0, G_OPTION_ARG_STRING, &floptions.param_im,          "Imaginary part for the parameter",          NULL },
    { "threads",      't',  0, G_OPTION_ARG_INT,    &threads,                     "The number of threads to use",              NULL },
    { "show-axes",    '\0', 0, G_OPTION_ARG_NONE,   &foptions.plot.show_axes,     "Show real and imaginary axes",              NULL },
    { NULL,           '\0', 0, G_OPTION_ARG_NONE,   (void*) NULL,                 "",                                          NULL }
};

GOptionEntry help_colour[] =
{
    { "show-colour",  'c',  0, G_OPTION_ARG_NONE,   &ioptions.colour.show_colour,   "Show colour for points not in the set",   NULL },
    { "inv-colour",   'C',  0, G_OPTION_ARG_NONE,   &ioptions.colour.inv_colour,    "Invert all colours",                      NULL },
    { "blue",         '\0', 0, G_OPTION_ARG_STRING, &floptions.bluescales,          "Brightness of blue componet (0 to 1)",    NULL },
    { "red",          '\0', 0, G_OPTION_ARG_STRING, &floptions.redscales,           "Brightness of red componet (0 to 1)",     NULL },
    { "green",        '\0', 0, G_OPTION_ARG_STRING, &floptions.greenscales,         "Brightness of green componet (0 to 1)",   NULL },
    { "smooth",       's',  0, G_OPTION_ARG_NONE,   &ioptions.colour.smooth_colour, "Smooth colours",                          NULL },
    { NULL,           '\0', 0, G_OPTION_ARG_NONE,   (void*) NULL,                   "",                                         NULL }
};

GOptionEntry help_mandelbrot[] =
{
    { "mandy-pow",    'p',  0, G_OPTION_ARG_INT,    &foptions.mandelbrot.power,     "The power to raise z to (default 2)",       NULL },
    { "mandy-conj",   'M',  0, G_OPTION_ARG_NONE,   &foptions.mandelbrot.conjugate, "Use complex conjugate of each z",           NULL },
    { NULL,           '\0', 0, G_OPTION_ARG_NONE,   (void*) NULL,                   "",                                          NULL }
};

GOptionEntry help_julia[] =
{
    { "julia",        'j',  0, G_OPTION_ARG_NONE,   &foptions.julia.julia,          "Generate a Julia set",                      NULL },
    { NULL,           '\0', 0, G_OPTION_ARG_NONE,   (void*) NULL,                   "",                                          NULL }
};

void do_params(int argc, char *argv[])
{
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

    /* Now for default options */
    if(!ioptions.image.width)      ioptions.image.width      = 1000;
    if(!ioptions.image.height)     ioptions.image.height     = 800;
    if(!ioptions.image.file)       ioptions.image.file       = (char*) "fractal.png";
    if(!foptions.mandelbrot.power) foptions.mandelbrot.power = 2;
    if(!foptions.misc.iterations)  foptions.misc.iterations  = 30;

    foptions.plot.re_min = -2.0;
    foptions.plot.re_max =  1.0;
    foptions.plot.im_min = -1.2;

    /* I didn't notice a way to do floats with glib option parsing, hence the 'floptions' struct */
    if(floptions.param_re)    foptions.misc.param[0]     = (float)atof(floptions.param_re);
    if(floptions.param_im)    foptions.misc.param[1]     = (float)atof(floptions.param_im);
    if(floptions.re_mins)     foptions.plot.re_min       = (float)atof(floptions.re_mins);
    if(floptions.re_maxs)     foptions.plot.re_max       = (float)atof(floptions.re_maxs);
    if(floptions.im_mins)     foptions.plot.im_min       = (float)atof(floptions.im_mins);
    if(floptions.redscales)   ioptions.colour.redscale   = (float)atof(floptions.redscales);
    if(floptions.greenscales) ioptions.colour.greenscale = (float)atof(floptions.greenscales);
    if(floptions.bluescales)  ioptions.colour.bluescale  = (float)atof(floptions.bluescales);
}

void *spawn_fractal_generator(void* arg)
{
    thread_options_t *toptions = (thread_options_t*)arg;

    fractal_main_loop(toptions->foptions, toptions->ioptions);

    pthread_exit(0);
}

void do_range_options(fractal_options_t *fopts, int isthread)
{
    fopts->plot.re_range  = fopts->plot.re_max - fopts->plot.re_min;

    if(!isthread)
    {
	fopts->plot.re_factor = fopts->plot.re_range / (float)(ioptions.image.width - 1);
	fopts->plot.im_max    = fopts->plot.im_min + fopts->plot.re_range * ((float)ioptions.image.height / (float)ioptions.image.width);
	fopts->plot.im_range  = fopts->plot.im_max - fopts->plot.im_min;
	fopts->plot.im_factor = fopts->plot.im_range / (float)(ioptions.image.height - 1);
    }
}

int main(int argc, char *argv[])
{
    do_params(argc, argv);

    do_range_options(&foptions, 0);

    pthread_t        *threadid = (pthread_t*)malloc(sizeof(pthread_t) * threads);
    thread_options_t *toptions = (thread_options_t*)malloc(sizeof(thread_options_t) * threads);

    int i;
    float slice = (float)foptions.plot.re_range / (float)threads;

    for(i = 0; i < threads; ++i)
    {
	toptions[i].ioptions = ioptions;
	toptions[i].foptions = foptions;

	toptions[i].ioptions.image.width  = toptions[i].ioptions.image.width / threads;
	toptions[i].foptions.plot.re_min += slice * i;
	toptions[i].foptions.plot.re_max -= slice * (threads - i - 1);

	/*printf("Thread %i: Re %f - %f, Im: %f - %f\n",
	       i,
	       toptions[i].foptions.plot.re_min, toptions[i].foptions.plot.re_max,
	       toptions[i].foptions.plot.im_min, toptions[i].foptions.plot.im_max);*/
	
	do_range_options(&toptions[i].foptions, 1);
    }

    for(i = 0; i < threads; ++i)
    {
	init_gd(&toptions[i].ioptions);
	pthread_create(&threadid[i], NULL, spawn_fractal_generator, (thread_options_t*)&toptions[i]);
    }

    for(i = 0; i < threads; ++i) pthread_join(threadid[i], NULL);

    merge_and_save_sets(toptions, threads, toptions[0].ioptions.image.file);

    return 0;
}
