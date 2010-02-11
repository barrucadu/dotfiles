#ifndef __MAIN_H
#define __MAIN_H

/* Macros */
#define logn(x,b) (log(x) / log(b))

/* Typedefs */
typedef float cmplx[2];
typedef int   coord[2];
typedef int   colour[3];

/* Structs */
typedef struct
{
    struct
    {
	int   width;
	int   height;
	char* file;
    } image;

    struct
    {
	int   show_colour;
	int   smooth_colour;
	int   inv_colour;
	float bluescale;
	float redscale;
	float greenscale;
    } colour;

    struct
    {
	gdImagePtr im;
    } gd;
} image_options_t;

typedef struct
{
    struct
    {
	int   power;
	int   conjugate;
    } mandelbrot;

    struct
    {
	int   julia;
    } julia;

    struct
    {
	float re_min;
	float re_max;
	float re_range;
	float re_factor;
	float im_min;
	float im_max;
	float im_range;
	float im_factor;
	int   show_axes;
    } plot;

    struct
    {
	cmplx param;
	int   iterations;
    } misc;
} fractal_options_t;

typedef struct
{
    char* re_mins;
    char* re_maxs;
    char* im_mins;
    char* param_re;
    char* param_im;
    char* redscales;
    char* greenscales;
    char* bluescales;
} float_options_t;

typedef struct topt
{
    image_options_t   ioptions;
    fractal_options_t foptions;
} thread_options_t;

/* Prototypes */
void do_params(int argc, char *argv[]);
void *spawn_fractal_generator(void* arg);
void do_range_options(fractal_options_t *fopts, int isthread);
int main(int argc, char *argv[]);

#endif
