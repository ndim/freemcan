/** \file hostware/freemcan-gui-device.c
 * \brief Freemcan GUI device event hookup (implementation)
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
 */


#include <assert.h>
#include <glib.h>
#include <glib-object.h>

#include "compiler.h"

#include "freemcan-device.h"
#include "freemcan-log.h"

#include "freemcan-gui-device.h"


/** \defgroup freemcan_gui_device Device Event Hookups
 * \ingroup freemcan_gui
 *
 * @{
 */


/** Set by gtk_init_with_args if --device on command line */
gchar *init_device_fname = NULL;


/** \bug Use init function hook into gtk_main */

static
gboolean gui_device_read_data_cb (GIOChannel *source,
				  GIOCondition condition,
				  gpointer UP(data))
{
  fmlog("%s(%p,0x%04x=%d,%p)", __PRETTY_FUNCTION__,
	(void*)source, condition, condition, _UP(data));
  if (source && (condition == G_IO_IN)) {
    const int fd = g_io_channel_unix_get_fd(source);
    device_do_io(fd);
    return TRUE;
  } else {
    return FALSE;
  }
}


GIOChannel *ioc = NULL;


void gui_device_command(const frame_cmd_t cmd, const uint16_t param)
{
  fmlog("%s('%c', 0x%04x=%d)", __PRETTY_FUNCTION__, cmd, param, param);
  if (ioc) {
    const int fd = g_io_channel_unix_get_fd(ioc);
    if (fd >= 0) {
      device_send_command(fd, cmd, param);
      return;
    }
  }
  fmlog("%s: could NOT sent command", __PRETTY_FUNCTION__);
}


void gui_device_update(const char *device_name)
{
  if (device_name) {
    if (ioc) {
      g_io_channel_shutdown(ioc, FALSE, NULL);
      g_io_channel_unref(ioc);
      ioc = NULL;
    }
    const int fd = device_open(device_name);
    if (fd >= 0) {
      ioc = g_io_channel_unix_new(fd);
      g_io_channel_set_encoding(ioc, NULL, NULL);
      g_io_channel_set_buffered(ioc, FALSE);
      g_io_add_watch_full(ioc, 0, G_IO_IN,
			  gui_device_read_data_cb, NULL,
			  NULL);
    }
  }
}


gboolean gui_device_init_func(gpointer UP(data))
{
  fmlog("%s(%p)", __PRETTY_FUNCTION__, _UP(data));
  fmlog("init_device_fname = %s", init_device_fname);
  gui_device_update(init_device_fname);
  return TRUE;
}


/** @} */
