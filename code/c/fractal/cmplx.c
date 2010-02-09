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

float cmplx_mod(cmplx z)
{
    float tmp;
    tmp = z[0] * z[0] + z[1] * z[1];

    return tmp;
}

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

void cmplx_conj(cmplx z, float *re, float *im)
{
    *re = z[0];
    *im = z[1] * -1;
}

int cmplx_greater(cmplx a, cmplx b)
{
    if(a[1] == b[1]) return (a[0] > b[0]);
    
    return -1; /* Must have same imaginary bit */
}

int cmplx_less(cmplx a, cmplx b)
{
    if(a[1] == b[1]) return (a[0] < b[0]);
    
    return -1;
}

int cmplx_equal(cmplx a, cmplx b)
{
    if(a[1] == b[1]) return (a[0] == b[0]);
    
    return -1;
}

void cmplx_add(cmplx a, cmplx b, float *re, float *im)
{
    *re = a[0] + b[0];
    *im = a[1] + b[1];
}

void cmplx_sub(cmplx a, cmplx b, float *re, float *im)
{
    *re = a[0] - b[0];
    *im = a[1] - b[1];
}
