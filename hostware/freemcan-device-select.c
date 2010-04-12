/** \file hostware/freemcan-device-select.c
 * \brief FreeMCAn device select(2) support (implementation)
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
 */


#include <assert.h>

#include "freemcan-device.h"
#include "freemcan-device-select.h"
#include "freemcan-tui.h"


/**
 * \defgroup freemcan_device_select Device Handling for select(2) based main loop (Layer 1)
 * \ingroup mainloop_select
 * \ingroup freemcan_device
 * @{
 */


device_t *device = NULL;


/* documented in dfreemcan-device-select.h */
int device_select_set_in(fd_set *in_fdset, int maxfd)
{
  assert(device);
  const int device_fd = device_get_fd(device);
  assert(device_fd >= 0);
  FD_SET(device_fd, in_fdset);
  if (device_fd > maxfd) {
    return device_fd;
  }
  else return maxfd;
}


/* documented in dfreemcan-device-select.h */
void device_select_do_io(fd_set *in_fdset)
{
  assert(device);
  const int device_fd = device_get_fd(device);
  assert(device_fd >= 0);
  if (FD_ISSET(device_fd, in_fdset)) {
    device_do_io(device);
  }
}


void device_select_init(const char *device_name)
{
  if (!device) {
    /** \bug HACK: Properly initialize parsers and stuff */
    frame_parser_t *fp = frame_parser_new();
    device = device_new(fp);
  }
  device_open(device, device_name);
  assert(device_get_fd(device) >= 0);
}


void device_select_fini()
{
  device_close(device);
}


void send_command(const frame_cmd_t cmd, const uint16_t param)
{
  device_send_command(device, cmd, param);
}


static void init(void) __attribute__((constructor));
static void init(void)
{
  tui_device_send_command = send_command;
}

/** @} */
