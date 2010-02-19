/** \file freemcan-main.c
 * \brief Main program for interactive interface to freemcan hardware
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
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

#include <sys/ioctl.h>
#include <errno.h>


/************************************************************************
 * Debug helpers
 ************************************************************************/


/** Print debug string */
#define DEBUG(...)				\
  do {						\
    fprintf(stderr, __VA_ARGS__);		\
  } while (0)


/** Return a printable character */
char printable(const char ch)
{
  if ((32 <= ch) && (ch < 127)) {
    return ch;
  } else {
    return '.';
  }
}


/* Print hexdump of data block */
void hexdump(const char *buf, const size_t size)
{
  const uint8_t *b = (const uint8_t *)buf;
  for (size_t y=0; y<size; y+=16) {
    DEBUG("%04x ", y);
    for (int x=0; x<16; x++) {
      if (y+x<size) {
	DEBUG(" %02x", b[y+x]);
      } else {
	DEBUG("   ");
      }
    }
    DEBUG("  ");
    for (size_t x=0; x<16; x++) {
      if (y+x<size) {
	DEBUG("%c", printable(b[y+x]));
      } else {
	DEBUG(" ");
      }
    }
    DEBUG("\n");
  }
}


/************************************************************************
 * select(2) helpers
 ************************************************************************/


/** Data size you can read from file descriptor without blocking */
static int read_size(const int in_fd)
{
  int bytes_to_read;
  int r = ioctl(in_fd, FIONREAD, &bytes_to_read);
  if (r < 0) {
    DEBUG("cannot determine number of characters to read from stdin");
    abort();
  }
  return bytes_to_read;
}


/************************************************************************
 * Device
 ************************************************************************/


static int device_fd = -1;


/** Open device */
void dev_init(const char *device_name)
{
  /* FIXME: Run stat(2) on device_name, then change open()-like code
   *        according to file type (device file, unix domain socket).
   */
  struct stat sb;
  const int stat_ret = stat(device_name, &sb);
  if (stat_ret == -1) {
    perror("stat()");
    abort();
  }
  if (S_ISCHR(sb.st_mode)) {
    DEBUG("%s: character device\n", device_name);
    device_fd = open(device_name, O_NOCTTY|O_RDWR);
  } else if (S_ISSOCK(sb.st_mode)) {
    DEBUG("%s: socket\n", device_name);
    const int sock = socket(AF_UNIX, SOCK_STREAM, 0);
    struct sockaddr_un addr;
    addr.sun_family = AF_UNIX;
    assert(strlen(device_name) < sizeof(addr.sun_path));
    strcpy(addr.sun_path, device_name);
    const int connect_ret = connect(sock, (const struct sockaddr *)&addr, sizeof(addr));
    if (connect_ret < 0) {
      perror("connect");
      abort();
    }
    device_fd = sock;
  } else {
    DEBUG("unknown?\n");
    abort();
  }
  assert(device_fd > 0);
  DEBUG("device_fd = %d\n", device_fd);
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
      DEBUG("EOF via device fd %d\n", device_fd);
      abort();
    }
    assert(bytes_to_read > 0);
    char buf[bytes_to_read+1];
    const ssize_t read_bytes = read(device_fd, buf, bytes_to_read);
    assert(read_bytes == bytes_to_read);
    buf[bytes_to_read] = '\0';
    DEBUG("Received %d bytes from fd %d: %s\n", read_bytes, device_fd, buf);
    hexdump(buf, read_bytes);
  }
}


/************************************************************************
 * (ncurses based) text UI
 ************************************************************************/


/** Set up select() data structure with (ncurses based) text UI */
int ui_select_set_in(fd_set *in_fdset, int maxfd)
{
  FD_SET(STDIN_FILENO, in_fdset);
  if (STDIN_FILENO > maxfd) return STDIN_FILENO;
  else return maxfd;
}


/** Do (ncurses based) text UI's IO stuff if necessary (from select loop) */
void ui_select_do_io(fd_set *in_fdset)
{
  if (FD_ISSET(STDIN_FILENO, in_fdset)) {
    const int bytes_to_read = read_size(device_fd);
    if (bytes_to_read == 0) {
      DEBUG("EOF from stdin, exiting.\n");
      exit(0);
    }
    assert(bytes_to_read > 0);
    char buf[bytes_to_read+1];
    const ssize_t read_bytes = read(STDIN_FILENO, buf, sizeof(buf));
    assert(read_bytes == bytes_to_read);
    buf[bytes_to_read] = '\0';
    DEBUG("Received %d bytes from fd %d: %s\n", read_bytes, STDIN_FILENO, buf);
    hexdump(buf, read_bytes);
    DEBUG("Copying data to fd %d\n", device_fd);
    write(device_fd, buf, sizeof(buf));
  }
}


/************************************************************************
 * Main loop
 ************************************************************************/


/* Next up: char-by-char input */

/** select(2) based main loop */
int main(int argc, char *argv[])
{
  assert(argc == 2);
  assert(argv[1] != NULL);
  assert(isatty(STDIN_FILENO));
  assert(isatty(STDOUT_FILENO));
  const char *device_name = argv[1];
  DEBUG("freemcan-main: device=%s\n", device_name);

  dev_init(device_name);

  while (1) {
    fd_set in_fdset;
    FD_ZERO(&in_fdset);
    FD_SET(device_fd, &in_fdset);
    int max_fd = -1;
    max_fd = ui_select_set_in(&in_fdset, max_fd);
    max_fd = dev_select_set_in(&in_fdset, max_fd);
    assert(max_fd >= 0);
    const int n = select(max_fd+1, &in_fdset, NULL, NULL, NULL);
    if (n<0) { /* error */
      if (errno != EINTR) {
	perror("select");
	abort();
      }
    } else if (0 == n) { /* timeout */
      DEBUG("select timeout\n");
      abort();
    } else { /* n>0 */
      dev_select_do_io(&in_fdset);
      ui_select_do_io(&in_fdset);
    }
  }

  return 0;
}
