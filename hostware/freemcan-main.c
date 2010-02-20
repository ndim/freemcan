/** \file freemcan-main.c
 * \brief Obsolete main program
 *
 * \bug Bury this file.
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
#include <unistd.h>

#include <sys/ioctl.h>
#include <errno.h>

#include "serial-setup.h"
#include "freemcan-log.h"
#include "freemcan-device.h"
#include "freemcan-tui.h"


/************************************************************************
 * Main loop
 ************************************************************************/


/* Next up: char-by-char input */

/** select(2) based main loop (obsolete) */
int main(int argc, char *argv[])
{
  assert(argv[0]);
  if (argc != 2) {
    fmlog("Fatal: Wrong command line parameter count.\n"
	  "\n"
	  "Synopsis:\n"
	  "    %s <serial-port-device>\n",
	  argv[0]);
    abort();
  }
  assert(argc == 2);
  assert(argv[1]);
  assert(isatty(STDIN_FILENO));
  assert(isatty(STDOUT_FILENO));

  const char *device_name = argv[1];
  fmlog("freemcan-main: device=%s\n", device_name);

  tui_init();
  dev_init(device_name);

  while (1) {
    fd_set in_fdset;
    FD_ZERO(&in_fdset);
    int max_fd = -1;
    max_fd = tui_select_set_in(&in_fdset, max_fd);
    max_fd = dev_select_set_in(&in_fdset, max_fd);
    assert(max_fd >= 0);
    const int n = select(max_fd+1, &in_fdset, NULL, NULL, NULL);
    if (n<0) { /* error */
      if (errno != EINTR) {
	fmlog_error("select");
	abort();
      }
    } else if (0 == n) { /* timeout */
      fmlog("select timeout\n");
      abort();
    } else { /* n>0 */
      dev_select_do_io(&in_fdset);
      tui_select_do_io(&in_fdset);
    }
  }

  return 0;
}
