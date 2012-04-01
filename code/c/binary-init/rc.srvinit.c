/* rc.srvinit.c - responsible for starting system services (daemons) */

#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/wait.h>
#include <sys/stat.h>

#include "rc.h"
#include "functions.h"

void start_daemons(char* daemons[], int background, int silent)
{
    /* Urgh, no idea where to start */
}

int findnet()
{
    /* Check output of iwlist */
    return 1;
}

int inschool()
{
    /* Check /proc/cmdline (fscanf?) */
    return 1;
}

int main(int argc, char *argv[])
{
    (void) argc;
    (void) argv;

    char* dimportant[] = DIMPORTANT;
    char* dnetwork[]   = DNETWORK;
    char* dschool[]    = DSCHOOL;
    char* dnormal[]    = DNORMAL;

    printf("-> Starting important daemons.\n");
    start_daemons(dimportant, 0, 0);

    if(findnet())
    {
	printf("-> Starting network daemons.\n");
	start_daemons(dnetwork, 1, 0);
    } else {
	printf("-> Not starting network daemons.\n");
    }

    if(inschool())
    {
	printf("-> Starting school daemons.\n");
	start_daemons(dschool, 1, 0);
    } else {
	printf("-> Starting normal daemons.\n");
	start_daemons(dnormal, 1, 0);
    }

    return 0;
}
