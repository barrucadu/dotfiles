#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <glib.h>
#include <gtk/gtk.h>
#include <gdk-pixbuf/gdk-pixbuf.h>

#include "fviewer.h"

gboolean delete_event(GtkWidget *widget, GdkEvent *event, gpointer data)
{
    (void) widget;
    (void) event;
    (void) data;

    return FALSE;
}

void destroy(GtkWidget *widget, gpointer data)
{
    (void) widget;
    (void) data;

    gtk_main_quit();
}

int main(int argc, char *argv[])
{
    GtkWindow *window;
    GtkWidget *hbox;

    gtk_init(&argc, &argv);
    
    /* ----- Window ----- */
    window = (GtkWindow*) gtk_window_new(GTK_WINDOW_TOPLEVEL);

    g_signal_connect(G_OBJECT(window), "delete_event", G_CALLBACK(delete_event), NULL);
    g_signal_connect(G_OBJECT(window), "destroy",      G_CALLBACK(destroy),      NULL);

    gtk_container_set_border_width(GTK_CONTAINER(window), 5);

    /* ----- H Box ----- */
    hbox = gtk_hbox_new(FALSE, 0);

    gtk_widget_show((GtkWidget*) window);

    gtk_main();
    
    return 0;
}
