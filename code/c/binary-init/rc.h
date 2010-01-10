#define RTC_MAJOR "253"
#define DMESG_LVL "3"

extern char **environ;

int run(char* args[]);
void mount(char* type, char* device, char* location, char* options, int nomtab);
void remount(char* location, int ro, int nomtab);
void modprobe(char* module);
void echo(char* filename, char* string);
int main(int argc, char *argv[]);

/* Terminate these arrays with an empty string */
char* MODULES[]      = {"iwl4965", "acpi-cpufreq", "snd-hda-intel", "radeon", ""};
char* ACPI_MODULES[] = {"ac", "battery", "button", "fan", "processor", "thermal", ""};
