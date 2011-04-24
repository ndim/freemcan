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

#include "compiler.h"

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
  /* user interface do_io */
  if (FD_ISSET(STDIN_FILENO, in_fdset)) {
    tui_do_io();
  }
}


void tui_select_timeout(struct timeval *UP(tv))
{
  tui_do_timeout();
}


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


extern int waiting_for;


void tui_device_send_simple_command(const frame_cmd_t cmd)
{
  device_send_command(device, cmd);
  waiting_for++;
}


/** Parameter layout with a single uint16_t param and time_t token */
typedef struct {
  /* to be read by firmware, needs endianness conversion */
  uint16_t _a;

  /* sent back as-is, not interpreted by firmware in any way */
  time_t start_time;
} PACKED measure_params_16_t;


/** Parameter layout with two uint16_t params and time_t token */
typedef struct {
  /* to be read by firmware, needs endianness conversion */
  uint16_t _a;
  uint16_t _b;

  /* sent back as-is, not interpreted by firmware in any way */
  time_t start_time;
} PACKED measure_params_16_16_t;


void tui_device_send_command_16(const frame_cmd_t cmd,
                                const time_t ts,
                                const uint16_t a)
{
  measure_params_16_t params = {
    htole16(a),
    ts
  };
  device_send_command_with_params(device, cmd,
                                  &params, sizeof(params));
  waiting_for++;
}


void tui_device_send_command_16_16(const frame_cmd_t cmd,
                                   const time_t ts,
                                   const uint16_t a,
                                   const uint16_t b)
{
  measure_params_16_16_t params = {
    htole16(a),
    htole16(b),
    ts
  };
  device_send_command_with_params(device, cmd,
                                  &params, sizeof(params));
  waiting_for++;
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
  const char *device_name = main_init(argc, argv);

  /** initialize output module */
  tui_init();

  /** device init and setting up the "network stack" */
  frame_parser_t *fp = frame_parser_new(tui_packet_parser);
  device = device_new(fp);
  device_open(device, device_name);
  assert(device_get_fd(device) >= 0);

  /** startup messages */
  tui_startup_messages();

  tui_device_send_simple_command(FRAME_CMD_PERSONALITY_INFO);
  tui_device_send_simple_command(FRAME_CMD_STATE);

  /** main loop */
  while (1) {
    struct timeval tv = { .tv_sec = periodic_update_interval, .tv_usec = 0 };
    fd_set in_fdset;
    FD_ZERO(&in_fdset);

    int max_fd = -1;
    max_fd = tui_select_set_in(&in_fdset, max_fd);
    max_fd = device_select_set_in(&in_fdset, max_fd);
    assert(max_fd >= 0);

    const int n = select(max_fd+1, &in_fdset, NULL, NULL,
                         (periodic_update_flag)?(&tv):NULL);
    if (n<0) { /* error */
      if (errno != EINTR) {
        fmlog_error("select(2)");
        abort();
      }
    } else if (0 == n) { /* timeout */
      tui_select_timeout(&tv);
    } else { /* n>0 */
      device_select_do_io(&in_fdset);
      tui_select_do_io(&in_fdset);
    }

    if (sigint || sigterm || quit_flag) {
      break;
    }

  } /* main loop */

  /* clean up */
  device_unref(device);
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
