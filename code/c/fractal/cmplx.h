#ifndef __CMPLX_H
#define CMPLX_H

float cmplx_mod(cmplx z);
void cmplx_mul(cmplx z1, cmplx z2, float *re, float *im);
void cmplx_pow(cmplx z, int pow, float *re, float *im);
void cmplx_conj(cmplx z, float *re, float *im);
int cmplx_greater(cmplx a, cmplx b);
int cmplx_less(cmplx a, cmplx b);
int cmplx_equal(cmplx a, cmplx b);
void cmplx_add(cmplx a, cmplx b, float *re, float *im);
void cmplx_sub(cmplx a, cmplx b, float *re, float *im);

#endif
