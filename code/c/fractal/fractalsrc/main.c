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

/* TODO: Get rid of all these global variables! */

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

/* Externs defined in fractal.c */
extern int   power;
extern int   conjugate;
extern float re_min;
extern float re_max;
extern float re_range;
extern float re_factor;
extern float im_min;
extern float im_max;
extern float im_range;
extern float im_factor;
extern cmplx param;
extern int   iterations;
extern int   show_axes;
extern int   julia;

/* Option Stuff */
char* re_mins     = NULL;
char* re_maxs     = NULL;
char* im_mins     = NULL;
char* param_re    = NULL;
char* param_im    = NULL;
char* redscales   = NULL;
char* greenscales = NULL;
char* bluescales  = NULL;

/* Glib option parser variables */
GOptionEntry help_all[] =
{
    { "filename",     'f',  0, G_OPTION_ARG_STRING, &image_file,   "Filename to save to",                       NULL },
    { "image-width",  'W',  0, G_OPTION_ARG_INT,    &image_width,  "Generated image width",                     NULL },
    { "image-height", 'H',  0, G_OPTION_ARG_INT,    &image_height, "Generated image height",                    NULL },
    { "iterations",   'n',  0, G_OPTION_ARG_INT,    &iterations,   "Number of iterations to run on each point", NULL },
    { "re-min",       'r',  0, G_OPTION_ARG_STRING, &re_mins,      "Start value on the real axis",              NULL },
    { "re-max",       'R',  0, G_OPTION_ARG_STRING, &re_maxs,      "End value on the real axis",                NULL },
    { "im-min",       'i',  0, G_OPTION_ARG_STRING, &im_mins,      "Start value on the imaginary axis",         NULL },
    { "re",           'a',  0, G_OPTION_ARG_STRING, &param_re,     "Real part for the parameter",               NULL },
    { "im",           'b',  0, G_OPTION_ARG_STRING, &param_im,     "Imaginary part for the parameter",          NULL },
    { "show-axes",    '\0', 0, G_OPTION_ARG_NONE,   &show_axes,    "Show real and imaginary axes",              NULL },
    { NULL,           '\0', 0, G_OPTION_ARG_NONE,   (void*) NULL,  "",                                          NULL }
};

GOptionEntry help_colour[] =
{
    { "show-colour",  'c',  0, G_OPTION_ARG_NONE,   &show_colour,   "Show colour for points not in the set",   NULL },
    { "inv-colour",   'C',  0, G_OPTION_ARG_NONE,   &inv_colour,    "Invert all colours",                      NULL },
    { "blue",         '\0', 0, G_OPTION_ARG_STRING, &bluescales,    "Brightness of blue componet (0 to 1)",    NULL },
    { "red",          '\0', 0, G_OPTION_ARG_STRING, &redscales,     "Brightness of red componet (0 to 1)",     NULL },
    { "green",        '\0', 0, G_OPTION_ARG_STRING, &greenscales,   "Brightness of green componet (0 to 1)",   NULL },
    { "smooth",       's',  0, G_OPTION_ARG_NONE,   &smooth_colour, "Smooth colours",                          NULL },
    { NULL,           '\0', 0, G_OPTION_ARG_NONE,   (void*) NULL,  "",                                         NULL }
};

GOptionEntry help_mandelbrot[] =
{
    { "mandy-pow",    'p',  0, G_OPTION_ARG_INT,    &power,        "The power to raise z to (default 2)",       NULL },
    { "mandy-conj",   'M',  0, G_OPTION_ARG_NONE,   &conjugate,    "Use complex conjugate of each z",           NULL },
    { NULL,           '\0', 0, G_OPTION_ARG_NONE,   (void*) NULL,  "",                                          NULL }
};

GOptionEntry help_julia[] =
{
    { "julia",        'j',  0, G_OPTION_ARG_NONE,   &julia,        "Generate a Julia set",                      NULL },
    { NULL,           '\0', 0, G_OPTION_ARG_NONE,   (void*) NULL,  "",                                          NULL }
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

    if(param_re) param[0] = (float)atof(param_re);
    if(param_im) param[1] = (float)atof(param_im);

    if(re_mins) re_min = (float)atof(re_mins);
    if(re_maxs) re_max = (float)atof(re_maxs);
    if(im_mins) im_min = (float)atof(im_mins);
    
    if(redscales)   redscale   = (float)atof(redscales);
    if(greenscales) greenscale = (float)atof(greenscales);
    if(bluescales)  bluescale  = (float)atof(bluescales);
}

int main(int argc, char *argv[])
{
    do_params(argc, argv);

    re_range  = re_max - re_min;
    re_factor = re_range / (float)(image_width - 1);
    im_max    = im_min + re_range * ((float)image_height / (float)image_width);
    im_range  = im_max - im_min;
    im_factor = im_range / (float)(image_height - 1);

    init_im();

    fractal_main_loop();

    save_set();

    return 0;
}
