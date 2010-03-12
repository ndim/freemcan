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


int device_fd = -1;


/* documented in dfreemcan-device-select.h */
int device_select_set_in(fd_set *in_fdset, int maxfd)
{
  assert(device_fd > 0);
  FD_SET(device_fd, in_fdset);
  if (device_fd > maxfd) {
    return device_fd;
  }
  else return maxfd;
}


/* documented in dfreemcan-device-select.h */
void device_select_do_io(fd_set *in_fdset)
{
  assert(device_fd > 0);
  if (FD_ISSET(device_fd, in_fdset)) {
    device_do_io(device_fd);
  }
}


void device_select_init(const char *device_name)
{
  device_fd = device_open(device_name);
  assert(device_fd >= 0);
}


void device_select_fini()
{
  device_close(device_fd);
  device_fd = -1;
}


void send_command(const frame_cmd_t cmd, const uint16_t param)
{
  if (device_fd >= 0) {
    device_send_command(device_fd, cmd, param);
  }
}


static void init(void) __attribute__((constructor));
static void init(void)
{
  tui_device_send_command = send_command;
}

/** @} */
