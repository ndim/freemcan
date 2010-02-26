/** \file freemcan-device.c
 * \brief FreeMCAn device (implementation)
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
#include <stdbool.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>


#include "freemcan-device.h"
#include "freemcan-frame.h"
#include "freemcan-log.h"
#include "freemcan-select.h"
#include "serial-setup.h"


static int device_fd = -1;


static
void open_char_device(const char *device_name)
{
  fmlog("%s: opening character device %s", __PRETTY_FUNCTION__, device_name);
  device_fd = serial_open(device_name);
  serial_setup(device_fd, serial_string_to_baud("9600"));
}


static
void open_unix_socket(const char *socket_name)
{
  fmlog("%s: opening AF_UNIX socket %s", __PRETTY_FUNCTION__, socket_name);
  const int sock = socket(AF_UNIX, SOCK_STREAM, 0);
  struct sockaddr_un addr;
  addr.sun_family = AF_UNIX;
  assert(strlen(socket_name) < sizeof(addr.sun_path));
  strcpy(addr.sun_path, socket_name);
  const int connect_ret = connect(sock, (const struct sockaddr *)&addr, sizeof(addr));
  if (connect_ret < 0) {
    fmlog_error("connect");
    abort();
  }
  device_fd = sock;
}


void dev_init(const char *device_name)
{
  struct stat sb;
  const int stat_ret = stat(device_name, &sb);
  if (stat_ret == -1) {
    perror("stat()");
    abort();
  }
  if (S_ISCHR(sb.st_mode)) { /* open serial port to the hardware device */
    open_char_device(device_name);
  } else if (S_ISSOCK(sb.st_mode)) { /* open UNIX domain socket to the emulator */
    open_unix_socket(device_name);
  } else {
    fmlog("device of unknown type: %s", device_name);
    abort();
  }
  assert(device_fd > 0);
  fmlog("device_fd = %d", device_fd);
}


void dev_fini(void)
{
  assert(device_fd > 0);
  close(device_fd);
}


int dev_select_set_in(fd_set *in_fdset, int maxfd)
{
  assert(device_fd > 0);
  FD_SET(device_fd, in_fdset);
  if (device_fd > maxfd) return device_fd;
  else return maxfd;
}


void dev_select_do_io(fd_set *in_fdset)
{
  assert(device_fd > 0);
  if (FD_ISSET(device_fd, in_fdset)) {
    const int bytes_to_read = read_size(device_fd);
    if (bytes_to_read == 0) {
      fmlog("EOF via device fd %d", device_fd);
      abort();
    }
    assert(bytes_to_read > 0);
    char buf[bytes_to_read+1];
    const ssize_t read_bytes = read(device_fd, buf, bytes_to_read);
    assert(read_bytes == bytes_to_read);
    buf[bytes_to_read] = '\0';
    if (false) {
      /* Logging this by default becomes tedious quickly with larger
       * amounts of data, so we comment this out for now.
       */
      fmlog("Received %d bytes from device at fd %d", read_bytes, device_fd);
      fmlog_data(buf, read_bytes);
    }
    frame_parse_bytes(buf, read_bytes);
  }
}


void dev_command(const frame_cmd_t cmd, const uint16_t param)
{
  fmlog("Sending %c command to device (param=%d=0x%04x)",
	cmd, param, param);
  switch (cmd) {
  case FRAME_CMD_MEASURE:
    if (1) {
      checksum_reset();
      const uint8_t cmd8 = cmd;
      write(device_fd, &cmd8, 1);
      checksum_update(cmd8);
      const uint8_t byte0 = (param & 0xff);
      checksum_update(byte0);
      const uint8_t byte1 = ((param>>8) & 0xff);
      checksum_update(byte1);
      write(device_fd, &param, sizeof(param));
      checksum_write(device_fd);
    }
    break;
  default:
    write(device_fd, &cmd, 1);
    break;
  }
}

