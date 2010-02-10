#ifndef __MAIN_H
#define __MAIN_H

/* Macros */
#define logn(x,b) (log(x) / log(b))

/* Typedefs */
typedef float cmplx[2];
typedef int   coord[2];
typedef int   colour[3];

/* Prototypes */
void do_params(int argc, char *argv[]);
int main(int argc, char *argv[]);

#endif
