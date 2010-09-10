/*******************************************/
/* Velleman K8062 DMX controller for Linux */
/* Helper functions for VM116/K8062        */
/*                                         */
/* Compile with gcc -o dmxd dmxd.c -lusb   */
/* (c) Denis Moreaux 2008                  */
/* Denis Moreaux <vapula@endor.be>         */
/*                                         */
/*******************************************/


extern int *dmx_maxchannels;
extern int *dmx_shutdown;
extern int *dmx_caps;
extern int *dmx_channels;

void dmx_open();
void dmx_close();
