/** \file hostware/freemcan-tui-main-select.c
 * \brief TUI main program select(2) support (implementation)
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
#include <errno.h>
#include <unistd.h>

/* According to POSIX.1-2001 */
#include <sys/select.h>

/* According to earlier standards */
#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>

#include "freemcan-device.h"
#include "freemcan-log.h"
#include "freemcan-signals.h"
#include "freemcan-tui.h"

/**
 * \defgroup freemcan_tui_select TUI handling for select(2) based main loop
 * \ingroup hostware_tui
 * \ingroup mainloop_select
 */


/** Set up select(2) data structure with device data */
int  device_select_set_in(fd_set *in_fdset, int maxfd);

/** Do device's IO stuff if necessary (from select(2) loop) */
void device_select_do_io(fd_set *in_fdset);


/** Set up select() data structure with (ncurses based) text UI
 * \ingroup freemcan_tui_select
 */
int tui_select_set_in(fd_set *in_fdset, int maxfd)
{
  FD_SET(STDIN_FILENO, in_fdset);
  if (STDIN_FILENO > maxfd) return STDIN_FILENO;
  else return maxfd;
}


/** Do TUI's IO stuff if necessary (from select loop)
 * \ingroup freemcan_tui_select
 */
void tui_select_do_io(fd_set *in_fdset)
{
  /* check if there is data available on terminal. if so process the data */
  if (FD_ISSET(STDIN_FILENO, in_fdset)) {
    tui_do_io();
  }
}


/**
 * \defgroup freemcan_device_select Device Handling for select(2) based main loop (Layer 1)
 * \ingroup mainloop_select
 * \ingroup freemcan_device
 * @{
 */


device_t *device = NULL;



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


void device_select_do_io(fd_set *in_fdset)
{
  assert(device);
  const int device_fd = device_get_fd(device);
  assert(device_fd >= 0);
  /* check if there is data available on uart. if so process the data */
  if (FD_ISSET(device_fd, in_fdset)) {
    device_do_io(device);
  }
}


void tui_device_send_command(const frame_cmd_t cmd, const uint16_t param)
{
  device_send_command(device, cmd, param);
}


/** @} */


/************************************************************************/
/** \defgroup mainloop_select Main loop based on select(2)
 * \ingroup hostware_tui
 * @{
 */
/************************************************************************/


/** TUI's main program with select(2) based main loop */
int main(int argc, char *argv[])
{
  /* get device name from command line */
  const char *device_name = main_init(argc, argv);

   /* initialize output module
    *
    * - initialize tui specific terminal
    * - initialize tui_packet_parser with packet_parser_new() */
  tui_init();

  /* create a new frame parser and register the packet parser in the frame parser  */
  frame_parser_t *fp = frame_parser_new(tui_packet_parser);
  /* create the uart device and register the frame parser in the device structure  */
  device = device_new(fp);
  /* open serial port */
  device_open(device, device_name);
  assert(device_get_fd(device) >= 0);

  /* startup messages */
  tui_startup_messages();

  /* main loop */
  while (1) {
    fd_set in_fdset;
    /* set file descriptor to null (no action) */
    FD_ZERO(&in_fdset);

    int max_fd = -1;
    /* check for any changes in file descriptor numbers. max_fd is the number
       of the highest file descriptor */
    /* stdio file descriptor */
    max_fd = tui_select_set_in(&in_fdset, max_fd);
    /* file descriptor uart */
    max_fd = device_select_set_in(&in_fdset, max_fd);

    assert(max_fd >= 0);

    /* if there are changes either (a datastream arrived on uart or terminal) */
    const int n = select(max_fd+1, &in_fdset, NULL, NULL, NULL);
    if (n<0) { /* error */
      if (errno != EINTR) {
        fmlog_error("select(2)");
        abort();
      }
    } else if (0 == n) { /* timeout */
      fmlog("select(2) timeout");
      abort();
    } else { /* n>0 */
      /* find out where the change was coming from and proceed the stuff */
      device_select_do_io(&in_fdset);
      tui_select_do_io(&in_fdset);
    }

    if (sigint || sigterm || quit_flag) {
      break;
    }

  } /* main loop */

  /* clean up */
  device_unref(device);
  /* end of program - restore terminal */
  tui_fini();

  /* implicitly call atexit_func */
  exit(EXIT_SUCCESS);
}

/** @} */


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
