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
#include "freemcan-log.h"
#include "freemcan-select.h"
#include "serial-setup.h"


static int device_fd = -1;


/** Open device */
void dev_init(const char *device_name)
{
  /** \todo Run stat(2) on device_name, then change open()-like code
   *       according to file type (device file, unix domain socket).
   */
  struct stat sb;
  const int stat_ret = stat(device_name, &sb);
  if (stat_ret == -1) {
    perror("stat()");
    abort();
  }
  if (S_ISCHR(sb.st_mode)) {
    fmlog("%s: character device\n", device_name);
    device_fd = serial_open(device_name);
    serial_setup(device_fd, serial_string_to_baud("9600"));
  } else if (S_ISSOCK(sb.st_mode)) {
    fmlog("%s: socket\n", device_name);
    const int sock = socket(AF_UNIX, SOCK_STREAM, 0);
    struct sockaddr_un addr;
    addr.sun_family = AF_UNIX;
    assert(strlen(device_name) < sizeof(addr.sun_path));
    strcpy(addr.sun_path, device_name);
    const int connect_ret = connect(sock, (const struct sockaddr *)&addr, sizeof(addr));
    if (connect_ret < 0) {
      fmlog_error("connect");
      abort();
    }
    device_fd = sock;
  } else {
    fmlog("unknown?\n");
    abort();
  }
  assert(device_fd > 0);
  fmlog("device_fd = %d\n", device_fd);
}


/** Close device */
void dev_fini(void)
{
  assert(device_fd > 0);
  close(device_fd);
}


/** Set up select() data structure with device data */
int dev_select_set_in(fd_set *in_fdset, int maxfd)
{
  assert(device_fd > 0);
  FD_SET(device_fd, in_fdset);
  if (device_fd > maxfd) return device_fd;
  else return maxfd;
}


/** Do device's IO stuff if necessary (from select loop) */
void dev_select_do_io(fd_set *in_fdset)
{
  assert(device_fd > 0);
  if (FD_ISSET(device_fd, in_fdset)) {
    const int bytes_to_read = read_size(device_fd);
    if (bytes_to_read == 0) {
      fmlog("EOF via device fd %d\n", device_fd);
      abort();
    }
    assert(bytes_to_read > 0);
    char buf[bytes_to_read+1];
    const ssize_t read_bytes = read(device_fd, buf, bytes_to_read);
    assert(read_bytes == bytes_to_read);
    buf[bytes_to_read] = '\0';
    fmlog("Received %d bytes from fd %d: %s\n", read_bytes, device_fd, buf);
    fmlog_data(buf, read_bytes);
  }
}


/** Write data buffer to device as-is */
void dev_write(const char *buf, const size_t size)
{
  fmlog("Writing %d bytes to device fd %d\n", size, device_fd);
  write(device_fd, buf, size);
}

