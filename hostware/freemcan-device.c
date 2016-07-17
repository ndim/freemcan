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
#include <sys/uio.h>


#include "freemcan-checksum.h"
#include "freemcan-device.h"
#include "frame-parser.h"
#include "freemcan-log.h"
#include "freemcan-iohelpers.h"
#include "freemcan-tui.h"
#include "serial-setup.h"

#include "uart-defs.h"


/** Internals of opaque #device_t */
struct _device_t {
  unsigned int refs;
  int fd;
  frame_parser_t *frame_parser;
  checksum_t *checksum_output;
};


device_t *device_new(frame_parser_t *frame_parser)
{
  device_t *device = malloc(sizeof(*device));
  assert(device);
  device->refs = 1;
  device->fd = -1;
  device->frame_parser = frame_parser;
  device->checksum_output = checksum_new();
  return device;
}


void device_ref(device_t *self)
{
  assert(self->refs > 0);
  self->refs++;
}


void device_unref(device_t *self)
{
  assert(self->refs > 0);
  self->refs--;
  if (self->refs == 0) {
    frame_parser_unref(self->frame_parser);
    checksum_unref(self->checksum_output);
    free(self);
  }
}


int device_get_fd(device_t *self)
{
  return self->fd;
}


/** Open character special device file with proper setup (to hardware device) */
static
int open_char_device(const char *device_name)
{
  fmlog("%s: opening character device:", __func__);
  fmlog("    %s", device_name);
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
  fmlog("%s: opening AF_UNIX socket %s", __func__, socket_name);
  const int sock = socket(AF_UNIX, SOCK_STREAM, 0);
  if (sock < 0) {
    fmlog_error("socket(2)");
    return -1;
  }

  struct sockaddr_un addr;
  addr.sun_family = AF_UNIX;
  assert(strlen(socket_name) < sizeof(addr.sun_path));
  strcpy(addr.sun_path, socket_name);
  addr.sun_path[sizeof(addr.sun_path)-1] = '\0';

  const int connect_ret = connect(sock, (const struct sockaddr *)&addr, sizeof(addr));
  if (connect_ret < 0) {
    fmlog_error("connect(2)");
    close(sock);
    return -1;
  }

  return sock;
}


void device_open(device_t *self, const char *device_name)
{
  struct stat sb;
  const int stat_ret = stat(device_name, &sb);
  if (stat_ret == -1) {
    perror("stat()");
    abort();
  }
  if (self->fd > 0) {
    device_close(self);
  }
  if (S_ISCHR(sb.st_mode)) { /* open serial port to the hardware device */
    self->fd = open_char_device(device_name);
  } else if (S_ISSOCK(sb.st_mode)) { /* open UNIX domain socket to the emulator */
    self->fd = open_unix_socket(device_name);
  } else {
    fmlog("device of unknown type: %s", device_name);
    self->fd = -1;
  }
}


void device_close(device_t *self)
{
  assert(self->fd > 0);
  close(self->fd);
  self->fd = -1;
}


void checksum_update_iovec(checksum_t *cs, struct iovec *iov)
{
  const uint8_t *buf = iov->iov_base;
  const size_t  size = iov->iov_len;
  for (size_t i=0; i<size; i++) {
    checksum_update(cs, buf[i]);
  }
}


extern bool enable_layer1_dump;

ssize_t my_writev(int fd, const struct iovec *iov, int iovcnt)
{
  if (enable_layer1_dump) {
    size_t size = 0;
    for (int i=0; i<iovcnt; i++) {
      size += iov[i].iov_len;
    }
    uint8_t *buf = malloc(size);
    assert(buf != NULL);
    for (ssize_t i=0, ofs=0; i<iovcnt; i++) {
      memcpy(&buf[ofs], iov[i].iov_base, iov[i].iov_len);
      ofs += iov[i].iov_len;
    }
    fmlog(">Sending 0x%04zx=%zd bytes of layer 1 data", size, size);
    fmlog_data(">>", buf, size);
    free(buf);
  }
  if (enable_layer2_dump) {
    static bool done_once = false;
    if (!done_once) {
      fmlog(">Sending data (layer 2 dump not implemented yet, use layer 1 dump for sending)");
      done_once = true;
    }
  }
  return writev(fd, iov, iovcnt);
}


void device_send_commandv(device_t *self, const frame_cmd_t cmd,
                          const struct iovec *iov, const int iovcnt)
{
  const int fd = self->fd;

  /* Debug output */
  if (fd > 0) {
    if (iov && iovcnt) {
      fmlog(">Sending '%c' command with iov to device", cmd);
      for (int i=0; i<iovcnt; i++) {
        fmlog_data("> * ", iov[i].iov_base, iov[i].iov_len);
      }
    } else {
      fmlog(">Sending '%c' command (without params) to device", cmd);
    }
  } else {
    if (iov && iovcnt) {
      fmlog("|Not sending '%c' command with iov to closed device", cmd);
      for (int i=0; i<iovcnt; i++) {
        fmlog_data("| * ", iov[i].iov_base, iov[i].iov_len);
      }
    } else {
      fmlog("|Not sending '%c' command (without params) to closed device", cmd);
    }
    return;
  }

  checksum_t *cs = checksum_new();

  /* Top of #out stack. Value is the number of elements on the #out
   * stack plus one. */
  int iovmax = 0;

  /* The place to compose the four mandatory frame parts plus optional
   * params */
  struct iovec out[4+iovcnt];

  /* First part of frame: magic string (mandatory) */
  out[iovmax].iov_base = (void *) FRAME_MAGIC_STR;
  out[iovmax].iov_len  = 4;
  iovmax++;

  /* Second part of frame: command (mandatory) */
  const uint8_t cmd8 = cmd;
  out[iovmax].iov_base = (void *) &cmd8;
  out[iovmax].iov_len  = 1;
  iovmax++;

  /* Third part of frame: length of parameters in bytes (mandatory)
   * The exact value of #len8 depends on the given params and thus
   * will be calculated a few lines down. */
  uint8_t len8 = 0;
  out[iovmax].iov_base = (void *) &len8;
  out[iovmax].iov_len  = 1;
  iovmax++;

  /* Fourth part of frame: All command parameter values (optional) */
  if ((iov != NULL) && (iovcnt > 0)) {
    /* Copy given parameters over into output iovec array out[] */
    for (int n=0; n<iovcnt; n++) {
      out[iovmax] = iov[n];
      len8 += iov[n].iov_len;
      iovmax++;
    }
  } else if ((iov != NULL) && (iovcnt <= 0)) {
    abort(); /* We are being called with invalid parameters */
  } else if ((iov == NULL) && (iovcnt > 0)) {
    abort(); /* We are being called with invalid parameters */
  } else {
    /* No extra parameters given. Do not send any. */
  }

  /* Calculate checksum over all frame octets until now,
   * i.e. except the FCS itself. */
  for (int i=0; i<iovmax; i++) {
    checksum_update_iovec(cs, &out[i]);
  }
  const uint16_t fcs = checksum_get(cs);
  checksum_unref(cs);

  /* Last part of frame: Frame Check Sequence (FCS) aka checksum */
  const uint16_t _fcs = htole16(fcs);
  out[iovmax].iov_base = (void *) &_fcs;
  out[iovmax].iov_len  = sizeof(_fcs);
  iovmax++;

  /* Write frame */
  my_writev(fd, out, iovmax);
}


/* documented in freemcan-device.h */
void device_do_io(device_t *self)
{
    const int fd = self->fd;
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
      fmlog("<Received %zd bytes from device at fd %d", read_bytes, fd);
      fmlog_data("<<", buf, read_bytes);
    }
    frame_parser_handle_bytes(self->frame_parser,
                              buf, read_bytes);
}


/** @} */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
