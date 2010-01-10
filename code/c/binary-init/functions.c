#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/wait.h>
#include <sys/stat.h>

#include "rc.h"
#include "functions.h"

/* Todo: done/fail function; colour */

int run(char* cmd)
{
    /* Todo: output redirection */

    int pid = fork();

    if(pid == 0)
    {
	if(VERBOSE)
	{
	    printf(":: %s\n", cmd);
	}

	char** args = calloc(25, 80 * sizeof(char *));
	char* cmdline = strdup(cmd);
	int i = 0;
	int errno;

	args[i] = strtok(cmdline, " ");
	while(args[i] != NULL)
	{
	    printf("%s ", args[i]);
	    i ++;
	    args[i] = strtok(NULL, " ");
	}
	args[i + 1] = NULL;

	errno = execv(args[0], args);
	free(cmdline);
	free(args);
	exit(errno);
    } else {
	int status;
	waitpid(pid, &status, 0);
	return status;
    }
}

void echo(char* filename, char* string, int append)
{
    FILE *file;

    if(append)
    {
	file = fopen(filename, "a+");
    } else {
	file = fopen(filename, "w+");
    }

    fprintf(file, "%s", string);
    fclose(file);
}

int fexists(char* path)
{
    struct stat buffer;
    int status = stat(path, &buffer);

    if(status == 0)
    {
	return 1;
    } else {
	return 0;
    }
}
