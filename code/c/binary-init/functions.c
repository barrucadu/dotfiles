#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <errno.h>
#include <fcntl.h>

#include "rc.h"
#include "functions.h"

/* Todo: done/fail function; colour */

int run(char* cmd, char* outfile)
{
    int pid = fork();

    if(pid == 0)
    {
	if(VERBOSE)
	{
	    printf("Run [%s]\n", cmd);
	}

	if(!strcmp(outfile, ""))
	{
	    int fd;
	    fd = open("dirlist.txt", O_RDWR | O_CREAT);
	    dup2(fd, STDOUT_FILENO);
	    dup2(fd, STDERR_FILENO);
	    close(fd);
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

	free(cmdline);
	free(args);

	if(execv(args[0], args) == -1)
	{
	    exit(errno);
	}
    } else {
	int status = 0;
	waitpid(pid, &status, 0);
	return WEXITSTATUS(status);
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
