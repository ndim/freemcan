/** \file hostware/freemcan-device.c
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
 *
 * \defgroup freemcan_device Device Interface
 * \ingroup hostware_generic
 *
 * \todo Use libftdi to interface to the USB->RS232 adapter?
 *
 * @{
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


#include "freemcan-checksum.h"
#include "freemcan-device.h"
#include "freemcan-frame.h"
#include "freemcan-log.h"
#include "freemcan-iohelpers.h"
#include "freemcan-tui.h"
#include "serial-setup.h"

#include "uart-defs.h"


send_command_f tui_device_send_command;


/** Open character special device file with proper setup (to hardware device) */
static
int open_char_device(const char *device_name)
{
  fmlog("%s: opening character device %s", __PRETTY_FUNCTION__, device_name);
  int fd = serial_open(device_name);
  if (fd < 0) {
    return -1;
  }
  serial_setup(fd, UART_BAUDRATE, 8, PARITY_NONE, 1);
  return fd;
}


/** Open AF_UNIX domain socket (to device emulator) */
static
int open_unix_socket(const char *socket_name)
{
  fmlog("%s: opening AF_UNIX socket %s", __PRETTY_FUNCTION__, socket_name);
  const int sock = socket(AF_UNIX, SOCK_STREAM, 0);
  if (sock < 0) {
    fmlog_error("socket(2)");
    return -1;
  }

  struct sockaddr_un addr;
  addr.sun_family = AF_UNIX;
  assert(strlen(socket_name) < sizeof(addr.sun_path));
  strcpy(addr.sun_path, socket_name);
  const int connect_ret = connect(sock, (const struct sockaddr *)&addr, sizeof(addr));
  if (connect_ret < 0) {
    fmlog_error("connect(2)");
    close(sock);
    return -1;
  }

  return sock;
}


int device_open(const char *device_name)
{
  struct stat sb;
  const int stat_ret = stat(device_name, &sb);
  if (stat_ret == -1) {
    perror("stat()");
    abort();
  }
  if (S_ISCHR(sb.st_mode)) { /* open serial port to the hardware device */
    return open_char_device(device_name);
  } else if (S_ISSOCK(sb.st_mode)) { /* open UNIX domain socket to the emulator */
    return open_unix_socket(device_name);
  } else {
    fmlog("device of unknown type: %s", device_name);
    return -1;
  }
}


void device_close(const int device_fd)
{
  assert(device_fd > 0);
  close(device_fd);
}


void device_send_command(const int fd,
			 const frame_cmd_t cmd, const uint16_t param)
{
  fmlog("Sending '%c' command to device (param=%d=0x%04x)",
	cmd, param, param);
  switch (cmd) {
  case FRAME_CMD_MEASURE:
    /* this is the only command with a parameter */
    if (1) {
      checksum_reset();
      const uint8_t cmd8 = cmd;
      write(fd, &cmd8, 1);
      checksum_update(cmd8);
      const uint8_t byte0 = (param & 0xff);
      checksum_update(byte0);
      const uint8_t byte1 = ((param>>8) & 0xff);
      checksum_update(byte1);
      write(fd, &param, sizeof(param));
      checksum_write(fd);
    }
    break;
  default:
    /* all other commands are without parameters */
    write(fd, &cmd, 1);
    break;
  }
}


/* documented in freemcan-device.h */
void device_do_io(const int fd)
{
    const int bytes_to_read = read_size(fd);
    if (bytes_to_read == 0) {
      fmlog("EOF via device fd %d", fd);
      abort();
    }
    assert(bytes_to_read > 0);
    char buf[bytes_to_read+1];
    const ssize_t read_bytes = read(fd, buf, bytes_to_read);
    assert(read_bytes == bytes_to_read);
    buf[bytes_to_read] = '\0';
    if (false) {
      /* Logging this by default becomes tedious quickly with larger
       * amounts of data, so we comment this out for now.
       */
      fmlog("Received %d bytes from device at fd %d", read_bytes, fd);
      fmlog_data(buf, read_bytes);
    }
    frame_parse_bytes(buf, read_bytes);
}


/** @} */
