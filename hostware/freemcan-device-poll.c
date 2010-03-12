/** \file hostware/freemcan-device-poll.c
 * \brief FreeMCAn device poll(2) support (implementation)
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
#include "freemcan-device-poll.h"
#include "freemcan-tui.h"


/**
 * \defgroup freemcan_device_poll Device Handling for poll(2) based main loop (Layer 1)
 * \ingroup mainloop_poll
 * \ingroup freemcan_device
 * @{
 */


int device_fd = -1;


void device_poll_init(const char *device_name)
{
  device_fd = device_open(device_name);
  assert(device_fd >= 0);
}


/** Handle available input detected by poll */
void device_poll_handler(struct pollfd *pfd)
{
  assert(pfd->fd > 0);
  if (pfd->revents & POLLIN) {
    device_do_io(pfd->fd);
  }
}


/* documented in dfreemcan-device.h */
void device_poll_setup(struct pollfd *pollfds, poll_handler_t *pollhandlers,
		    nfds_t *index, const nfds_t limit)
{
  assert(device_fd > 0);
  assert(index);
  assert(*index < limit);
  pollfds[*index].fd = device_fd;
  pollfds[*index].events = POLLIN;
  pollfds[*index].revents = 0;
  pollhandlers[*index] = device_poll_handler;
  (*index)++;
}


void device_poll_fini(void)
{
  device_close(device_fd);
  device_fd = -1;
}


static
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


