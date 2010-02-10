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

fractal_options_t foptions;
image_options_t   ioptions;
float_options_t   floptions;

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

int main(int argc, char *argv[])
{
    do_params(argc, argv);

    foptions.plot.re_range  = foptions.plot.re_max - foptions.plot.re_min;
    foptions.plot.re_factor = foptions.plot.re_range / (float)(ioptions.image.width - 1);
    foptions.plot.im_max    = foptions.plot.im_min + foptions.plot.re_range * ((float)ioptions.image.height / (float)ioptions.image.width);
    foptions.plot.im_range  = foptions.plot.im_max - foptions.plot.im_min;
    foptions.plot.im_factor = foptions.plot.im_range / (float)(ioptions.image.height - 1);

    ioptions.gd.im = init_gd(ioptions);

    fractal_main_loop(foptions, ioptions);

    save_set(ioptions);

    return 0;
}
