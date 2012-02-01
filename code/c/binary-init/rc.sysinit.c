/* rc.sysinit.c - responsible for basic system init (udev, mounting, etc) */

#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/wait.h>
#include <sys/stat.h>

#include "rc.h"
#include "functions.h"

int startudev()
{
    run("/sbin/udevd --daemon",  "/dev/null");
    run("/sbin/udevadm trigger", "/dev/null");

    return 1;
}

int modprobe()
{
    char* probecmd = malloc(512 * sizeof(char*));
    int i;

    char* modules[] = MODULES;
    i = 0;
    while(strcmp(modules[i], "") != 0)
    {
	strcpy(probecmd, "/sbin/modprobe ");
	strcat(probecmd, modules[i]);
	run(probecmd, "/dev/null");
	i ++;
    }

    char* acpi_modules[] = ACPI_MODULES;
    i = 0;
    while(strcmp(acpi_modules[i], "") != 0)
    {
	strcpy(probecmd, "/sbin/modprobe ");
	strcat(probecmd, acpi_modules[i]);
	run(probecmd, "/dev/null");
	i ++;
    }

    free(probecmd);
    return 1;
}

int loopbackup()
{
    run("/sbin/ifconfig lo 127.0.0.1 up", "/dev/null");
    return 1;
}

int mountroot()
{
    run("/bin/mount -n -o remount,ro /", "/dev/null");
    return 1;
}

int fsck()
{
    char* fsckcmd = malloc(512 * sizeof(char*));

    strcpy(fsckcmd, "/bin/fsck -A -T -C -a -t ");
    strcat(fsckcmd, NETFS);
    run(fsckcmd, "/dev/null");

    free(fsckcmd);
    return 1;
}

int mountall()
{
    char* mountcmd = malloc(512 * sizeof(char*));

    run("/bin/mount -n -o remount,rw /", "/dev/null");
    system("/bin/rm -f /etc/mtab* &>/dev/null");
    run("/bin/mount -o remount,rw /", "/dev/null");
    system("/bin/grep -e '/proc ' -e '/sys ' -e '/dev ' /proc/mounts >> /etc/mtab");
    strcpy(mountcmd, "/bin/mount -a -t ");
    strcat(mountcmd, NETFS);
    strcat(mountcmd, " -O no_netdev");
    run(mountcmd, "/dev/null");

    free(mountcmd);
    return 1;
}

int swapon()
{
    run("/sbin/swapon -a", "/dev/null");
    return 1;
}

int sysclock()
{
    char* clockcmd = malloc(512 * sizeof(char*));

    if(!fexists("/var/lib/hwclock/adjtime"))
    {
	echo("/var/lib/hwclock/adjtime", "0.0 0 0.0", 0);
    }

    char* timepath = malloc(80 * sizeof(char));
    strcpy(timepath, "/usr/share/zoneinfo/");
    strcat(timepath, TIMEZONE);

    if(!strcmp(TIMEZONE, "") && fexists(timepath))
    {
	remove("/etc/localtime");

	strcpy(clockcmd, "/bin/cp \"");
	strcat(clockcmd, timepath);
	strcat(clockcmd, "\" /etc/localtime");
	run(clockcmd, "/dev/null");
    }

    free(timepath);

    if(!strcmp(HWCLOCK_PARAMS, ""))
    {
	run("/sbin/hwclock --adjust", "/dev/null");

	strcpy(clockcmd, "/sbin/hwclock ");
	strcat(clockcmd, HWCLOCK_PARAMS);
	run(clockcmd, "/dev/null");
    }

    free(clockcmd);

    return 1;
}

int randomseed()
{
    char* seedcmd = malloc(512 * sizeof(char*));

    strcpy(seedcmd, "/bin/cat ");
    strcat(seedcmd, RANDOM_SEED);
    strcat(seedcmd, " > /dev/urandom");
    system(seedcmd);

    free(seedcmd);
    return 1;
}

int cleanfiles()
{
    remove("/etc/nologin");
    remove("/etc/shutdownpid");
    remove("/forcefsck");

    system("/bin/rm -f /var/lock/* &>/dev/null");
    system("/bin/rm -f /tmp/* /tmp/.* &>/dev/null");
    system("cd /var/run && /usr/bin/find . ! -type d -exec /bin/rm -f -- {} \\; &>/dev/null");

    echo("/var/run/utmp", "", 0);
    chmod("/var/run/utmp", S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH);

    mkdir("/tmp/.ICE-unix", S_ISVTX | S_IRWXU | S_IRWXG | S_IRWXO);
    mkdir("/tmp/.X11-unix", S_ISVTX | S_IRWXU | S_IRWXG | S_IRWXO);

    return 1;
}

int doldconfig()
{
    run("/sbin/ldconfig", "/dev/null");
    return 1;
}

int hostname()
{
    char* hostcmd = malloc(512 * sizeof(char*));

    strcpy(hostcmd, "/bin/hostname ");
    strcat(hostcmd, HOSTNAME);

    run(hostcmd, "/dev/null");

    free(hostcmd);

    return 1;
}

int dodepmod()
{
    run("/sbin/depmod -A", "/dev/null");
    return 1;
}

int dolocale()
{
    char* loccmd = malloc(512 * sizeof(char*));

    echo("/etc/profile.d/locale.sh", "", 0);
    chmod("/etc/profile.d/locale.sh", S_IRWXU | S_IRGRP | S_IXGRP | S_IROTH | S_IXOTH);

    strcpy(loccmd, "export LANG=");
    strcat(loccmd, LOCALE);
    echo("/etc/profile.d/locale.sh", loccmd, 0);

    free(loccmd);
    return 1;
}

int utfconsoles()
{
    /* Eww, using system for this is a messy work-around */
    system("for i in /dev/tty[0-9]*; do /usr/bin/kbd_mode -u < ${i}; printf \"\\e%%G\" > ${i}; done");
    echo("/etc/profile.d/locale.sh", "if [ \"$CONSOLE\" = \"\" -a \"$TERM\" = \"linux\" -a -t 1 ]; then printf \"\\e%%G\"; fi", 1);
    return 1;
}

int dokeymap()
{
    char* keycmd = malloc(512 * sizeof(char*));

    strcpy(keycmd, "/bin/loadkeys -q -u ");
    strcat(keycmd, KEYMAP);

    run(keycmd, "/dev/null");
    free(keycmd);

    return 1;
}

int dofont()
{
    char* fontcmd = malloc(512 * sizeof(char*));

    /* Another horrible work-around function */
    strcpy(fontcmd, "for i in /dev/tty[2-9]*; do /usr/bin/setfont ");
    if(strcmp(CONSOLEMAP, ""))
    {
	strcat(fontcmd, CONSOLEFONT);
	strcat(fontcmd, " -C ${i} >/dev/null 2>&1");
    } else {
	strcat(fontcmd, "-m ");
	strcat(fontcmd, CONSOLEMAP);
	strcat(fontcmd, " ");
	strcat(fontcmd, CONSOLEFONT);
	strcat(fontcmd, " -C ${i} >/dev/null 2>&1");
    }
    strcpy(fontcmd, "; done");
    system(fontcmd);

    free(fontcmd);
    return 1;
}

int main(int argc, char *argv[])
{
    (void) argc;
    (void) argv;

    char* cmd = malloc(512 * sizeof(char*));

    printf("\nArch Linux\n");
    printf("Copyright 2002-2007 Judd Vinet, 2007-2010 Aaron Griffin\n\n");

    /* Mount /dev, /proc, and /sys */
    run("/bin/mount -n -t tmpfs none /dev -o mode=0755", "/dev/null");
    run("/bin/mount -n -t proc none /proc", "/dev/null");
    run("/bin/mount -n -t sysfs none /sys", "/dev/null");

    /* Copy static device nodes to /dev */
    system("cp -a /lib/udev/devices/* /dev/ &>/dev/null");

    /* Start minlogd until syslog takes over */
    run("/sbin/minilogd", "/dev/null");

    /* Run dmesg; anything serious gets printed */
    strcpy(cmd, "/bin/dmesg -n ");
    strcat(cmd, DMESG_LVL);
    run(cmd, "/dev/null");

    /* Enable real-time clock access */
    run("modprobe rtc-cmos", "/dev/null");

    strcpy(cmd, "/bin/mknod /dev/rtc0 c ");
    strcat(cmd, RTC_MAJOR);
    strcat(cmd, " 0");
    run(cmd, "/dev/null");

    run("/bin/ln -s /dev/rtc0 /dev/rtc", "/dev/null");

    /* Set clock early */
    run("/sbin/hwclock --hctosys --localtime --noadjfile", "/dev/null");

    /* Disable hotplugging */
    echo("/proc/sys/kernel/hotplug", "", 0);

    /* Run UDev */
    printf("Starting UDev daemon...");
    if(startudev())
    {
	printf(" ...done!\n");
    } else {
	printf(" ...fail!\n");
    }

    /* Kernel modules */
    printf("Probing kernel modules...");
    if(modprobe())
    {
	printf(" ...done!\n");
    } else {
	printf(" ...fail!\n");
    }

    /* Finish UDev */
    run("/sbin/udevadm settle", "/dev/null");

    /* Loopback */
    printf("Bringing up loopback interface...");
    if(loopbackup())
    {
	printf(" ...done!\n");
    } else {
	printf(" ...fail!\n");
    }

    /* Mount root */
    printf("Mounting root filesystem read-only...");
    if(mountroot())
    {
	printf(" ...done!\n");
    } else {
	printf(" ...fail!\n");
    }

    /* fsck */
    /* Todo: add fsck failed / reboot required handling; forcefsck */
    printf("Performing filesystem consistency check...");
    if(fsck())
    {
	printf(" ...done!\n");
    } else {
	printf(" ...fail!\n");
    }

    /* Mount */
    printf("Mounting filesystems...");
    if(mountall())
    {
	printf(" ...done!\n");
    } else {
	printf(" ...fail!\n");
    }

    /* Swap */
    printf("Activating swap partition...");
    if(swapon())
    {
	printf(" ...done!\n");
    } else {
	printf(" ...fail!\n");
    }

    /* Clock */
    printf("Activating system clock...");
    if(sysclock())
    {
	printf(" ...done!\n");
    } else {
	printf(" ...fail!\n");
    }

    /* Random seed */
    if(fexists(RANDOM_SEED))
    {
	printf("Initialising random seed...");
	if(randomseed())
	{
	    printf(" ...done!\n");
	} else {
	    printf(" ...fail!\n");
	}
    }

    /* Leftovers */
    printf("Removing leftover boot files...");
    if(cleanfiles())
    {
	printf(" ...done!\n");
    } else {
	printf(" ...fail!\n");
    }

    /* Shared library links */
    if(DOLDCONFIG)
    {
	printf("Updating shared library links...");
	if(doldconfig())
	{
	    printf(" ...done!\n");
	} else {
	    printf(" ...fail!\n");
	}
    }

    /* Hostname */
    printf("Setting hostname...");
    if(hostname())
    {
	printf(" ...done!\n");
    } else {
	printf(" ...fail!\n");
    }

    /* Module deps */
    if(DODEPMOD)
    {
	printf("Updating module dependencies...");
	if(dodepmod())
	{
	    printf(" ...done!\n");
	} else {
	    printf(" ...fail!\n");
	}
    }

    /* Set locale */
    printf("Setting locale...");
    if(dolocale())
    {
	printf(" ...done!\n");
    } else {
	printf(" ...fail!\n");
    }

    printf("Setting consoles to UTF-8 mode...");
    if(utfconsoles())
    {
	printf(" ...done!\n");
    } else {
	printf(" ...fail!\n");
    }

    if(!strcmp(KEYMAP, ""))
    {
	printf("Setting keymap...");
	if(dokeymap())
	{
	    printf(" ...done!\n");
	} else {
	    printf(" ...fail!\n");
	}
    }

    printf("Loading console font...");
    if(dofont())
    {
	printf(" ...done!\n");
    } else {
	printf(" ...fail!\n");
    }

    system("/bin/dmesg >| /var/log/dmesg.log");

    free(cmd);
    return 0;
}
