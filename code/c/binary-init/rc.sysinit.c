#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/wait.h>
#include <glib.h>

#include "rc.h"

int run(char* args[])
{
    int pid = fork();
    
    if(pid == 0)
    {
	return execv(args[0], args);
    } else {
	int status;
	waitpid(pid, &status, 0);
	return status;
    }
}

void mount(char* type, char* device, char* location, char* options, int nomtab)
{
    int argn = 6;

    if(strcmp(options, "") > 0)
	argn += 1;

    if(nomtab == 1)
	argn += 1;

    char** args = calloc(argn, 256);
    args[0] = "/bin/mount";
    args[1] = "-t";
    args[2] = type;
    args[3] = device;
    args[4] = location;

    if(options)
    {
	args[5] = "-o";
	args[6] = options;

	if(nomtab == 1)
	{
	    args[7] = "-n";
	    args[8] = NULL;
	} else {
	    args[7] = NULL;
	}
    } else if(nomtab == 1) {
	args[5] = "-n";
	args[6] = NULL;
    } else {
	args[5] = NULL;
    }

    run(args);
    free(args);
}

void remount(char* location, int ro, int nomtab)
{
    char** args = calloc(5, 256);
    args[0] = "/bin/mount";
    args[1] = "-o";
    
    if(ro == 1)
    {
	args[2] = "remount,ro";
    } else {
	args[2] = "remount";
    }

    args[3] = location;

    if(nomtab == 1)
    {
	args[4] = "-n";
	args[5] = NULL;
    } else {
	args[4] = NULL;
    }

    run(args);
    free(args);
}

void modprobe(char* module)
{
    char** args = calloc(3, 256);
    args[0] = "/sbin/modprobe";
    args[1] = module;
    args[2] = NULL;

    run(args);
    free(args);
}

void echo(char* filename, char* string)
{
    FILE *file;
    file = fopen(filename, "w+");
    fprintf(file, "%s", string);
    fclose(file);
}

int main(int argc, char *argv[])
{
    (void) argc;
    (void) argv;

    int i;
    char** args = calloc(11, 256);
    
    printf("\nArch Linux\n");
    printf("Copyright 2002-2007 Judd Vinet, 2007-2010 Aaron Griffin\n\n");

    /* Mount /dev, /proc, and /sys */
    mount("tmpfs", "none", "/dev", "mode=0755", 1);
    mount("proc",  "none", "/proc", "", 0);
    mount("sysfs", "none", "/sys", "", 0);

    /* Copy static device nodes to /dev */
    system("cp -a /lib/udev/devices/* /dev/");

    /* Start minlogd until syslog takes over */
    args[0] = "/sbin/minlogd";
    args[1] = NULL;
    run(args);

    /* Run dmesg; anything serious gets printed */
    args[0] = "/bin/dmesg";
    args[1] = "-n";
    args[2] = DMESG_LVL;
    args[3] = NULL;
    run(args);

    /* Enable real-time clock access */
    modprobe("rtc-cmos");

    args[0] = "/bin/mknod";
    args[1] = "/dev/rtc0";
    args[2] = "c";
    args[3] = RTC_MAJOR;
    args[4] = "0";
    args[5] = NULL;
    run(args);

    args[0] = "/bin/ln";
    args[1] = "-s";
    args[2] = "/dev/rtc0";
    args[3] = "/dev/rtc";
    args[4] = NULL;
    run(args);

    /* Set clock early */
    args[0] = "/sbin/hwclock";
    args[1] = "--hctosys";
    args[2] = "--localtime";
    args[3] = "--noadjfile";
    args[4] = NULL;
    run(args);

    /* Disable hotplugging */
    echo("/proc/sys/kernel/hotplug", "");

    /* Run UDev */
    printf("Starting UDev daemon\n");
    args[0] = "/sbin/udevd";
    args[1] = "--daemon";
    args[2] = NULL;
    run(args);

    args[0] = "/sbin/udevadm";
    args[1] = "trigger";
    args[2] = NULL;
    run(args);

    /* Kernel modules */
    printf("Probing kernel modules\n");
    i = 0;
    while(strcmp(MODULES[i], "") != 0)
    {
	modprobe(MODULES[i]);
	i ++;
    }

    i = 0;
    while(strcmp(ACPI_MODULES[i], "") != 0)
    {
	modprobe(ACPI_MODULES[i]);
	i ++;
    }

    /* Finish UDev */
    args[0] = "/sbin/udevadm";
    args[1] = "settle";
    args[2] = NULL;
    run(args);

    /* Loopback */
    printf("Bringing up loopback interface\n");
    args[0] = "/sbin/ifconfig";
    args[1] = "lo";
    args[2] = "127.0.0.1";
    args[3] = "up";
    args[4] = NULL;
    run(args);

    /* Mount root */
    printf("Mounting root filesystem read-only\n");
    remount("/", 1, 1);
    return 0;
}
