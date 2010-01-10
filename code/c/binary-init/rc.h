/* Functional settings */

#define RTC_MAJOR   "253"
#define DMESG_LVL   "3"
#define VERBOSE      0
#define NETFS       "nonfs,nonfs4,nosmbfs,nocifs,nocodafs,noncpfs,nosysfs,noshfs,nofuse,nofuseblk,noglusterfs"
#define RANDOM_SEED "/var/lib/misc/random-seed"
#define DOLDCONFIG   0
#define DODEPMOD     0

/* General Settings */
#define TIMEZONE       "Europe/London"
#define HWCLOCK_PARAMS ""
#define HOSTNAME       "cthulhu"
#define LOCALE         "en_GB.utf8"
#define KEYMAP         "uk"
#define CONSOLEMAP     ""
#define CONSOLEFONT    "ter-112n"

/* Terminate these arrays with an empty string */
#define MODULES   {"iwl4965", "acpi-cpufreq", "snd-hda-intel", "radeon", ""}
#define ACPI_MODULES {"ac", "battery", "button", "fan", "processor", "thermal", ""}

extern char **environ;
