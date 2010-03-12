/** \file hostware/freemcan-gui-main.c
 * \brief Freemcan graphical user interface (GUI)
 *
 * \author Copyright (C) 2010 Hans Ulrich Niedermann <hun@n-dimensional.de>
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public License
 *  as published by the Free Software Foundation; either version 2.1
 *  of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free
 *  Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 *  Boston, MA 02110-1301 USA
 *
 * \defgroup freemcan_gui Graphical User Interface (GUI)
 * \ingroup hostware
 *
 * \bug Have GUI properly handle the emulator socket closing unexpectedly
 *
 * \bug Determine firmware/emulator state from its output, adapt GUI
 *      state accordingly.  Idea: Send 'i' cmd at beginning. If we get
 *      histogram back, the device is measuring. If we get a "READY"
 *      status back, there is a ready device. If neither happens, we
 *      are disconnected.
 *
 * \bug Add way to select certain data file as background level
 * \bug Periodically send 'i' command during measurement to update display
 * \bug Determine the the hardware state and show it in the GUI somehow
 * \bug GUI for setting directory to store exported data files to
 * \bug Use gconf or something similar to remember settings
 * \bug Add command line option for background level
 * \bug Show diagrams loaded from file
 * \bug Show multiple diagrams at once
 * \bug Show log messages in GUI window instead of console
 * \bug Add --no-gui parameter that makes freemcan-gui work in TTY just like freemcan-tui
 *
 * @{
 */


#include <assert.h>
#include <math.h>
#include <stdint.h>
#include <string.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include <cairo/cairo.h>
#include <gdk/gdk.h>
#include <gtk/gtk.h>
#include <gconf/gconf-client.h>

#include "compiler.h"

#include "freemcan-device.h"
#include "freemcan-log.h"
#include "freemcan-packet.h"

#include "fhistogram.h"
#include "fmainwindow.h"
#include "freemcan-gui-device.h"
#include "freemcan-gui-diagram.h"


/** GUI main function based on gtk_main() */
int main(int argc, char *argv[])
{
  GError *error = NULL;

  /** Must be called before gconf_* */
  g_type_init();

  /* Init GTK+ */
  GOptionEntry options[] = {
    {"device", 'd',
     G_OPTION_FLAG_FILENAME,
     G_OPTION_ARG_FILENAME,
     &init_device_fname,
     "serial interface to freemcan hardware (character device file)",
     "DEVICE"
    },
    {NULL, 0,
     0,
     0,
     NULL,
     NULL,
     NULL
    }
  };
  const gboolean init_ok =
    gtk_init_with_args(&argc, &argv,
		       "",
		       options, NULL, &error);
  if (!init_ok) {
    g_error("%s", error->message);
    // g_free(error);
    return 1;
  }

  GtkFMainWindow *gui = GTK_FMAINWINDOW(gtk_fmainwindow_new());
  assert(gui != NULL);

  /* Apply initial gconf settings */
  gtk_fmainwindow_init_device_name(gui, init_device_fname);

  /* Show window. All other widgets are automatically shown by GtkBuilder */
  gtk_widget_show(GTK_WIDGET(gui));

  /* start the event loop */
  gtk_init_add(gui_device_init_func, NULL);
  gtk_main();

  /* Destroy builder, since we don't need it anymore */
  g_object_unref(G_OBJECT(gui));

  return 0;
}


/** @} */
