/** \file freemcan-text.c
 * \brief Freemcan interactive text user interface (non-ncurses)
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


#include <stdbool.h>

#include <unistd.h>
#include <time.h>

#include <signal.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <assert.h>
#include <string.h>
#include <errno.h>

#include <unistd.h>

#include <termios.h>
#include <sys/ioctl.h>

#include "freemcan-common.h"
#include "freemcan-device.h"
#include "freemcan-frame.h"
#include "freemcan-log.h"
#include "freemcan-select.h"



static bool quit_flag = false;



/** Saved TTY fd */
static int tty_savefd = -1;

/** Saved termios data */
static struct termios tty_save_termios;


/** Put terminal into raw mode.
 *
 * Stevens, page 354, Program 11.10
 */
int tty_raw(int fd)
{
  struct termios buf;

  if (tcgetattr(fd, &tty_save_termios) < 0) {
    return -1;
  }

  buf = tty_save_termios;
  buf.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
  buf.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
  buf.c_cflag &= ~(CSIZE | PARENB);
  buf.c_cflag |= CS8;
  buf.c_oflag &= ~(OPOST);

  buf.c_cc[VMIN] = 1;
  buf.c_cc[VTIME] = 0;

  if (tcsetattr(fd, TCSAFLUSH, &buf) < 0) {
    return -1;
  }
  tty_savefd = fd;

  return 0;
}


void tty_init()
{
  assert(tty_raw(STDIN_FILENO) >= 0);
}


/** Restore terminal mode.
 *
 * Stevens, page 355, Program 11.10
 */
int tty_reset()
{
  if (tty_savefd <= 0) {
    return 0;
  }

  if (tcsetattr(tty_savefd, TCSAFLUSH, &tty_save_termios) < 0) {
    return -1;
  }

  return 0;
}


/** Handle ABRT signal */
static void sigabrt_handler(int i __attribute__((unused)))
{
  tty_reset();
  fprintf(stderr, "SIGABRT\n");
}


/** Initialize ABRT signal handling */
static void sigabrt_init()
{
  sighandler_t abrt_handler __attribute__((unused))
    = signal(SIGABRT, sigabrt_handler);
  /* linux will try and restart an interrupted system call by default */
  siginterrupt(SIGABRT, 1); /* stop system calls on SIGABRT */
}


/** Log file */
FILE *stdlog = NULL;


/** TUI specific message logger for #fmlog() & Co. */
static void
tui_log_handler(void *data __attribute__ (( unused )),
		const char *message,
		const size_t length __attribute__ (( unused )))
{
  fprintf(stdout, "%s\r\n", message);
  fflush(stdout);
  if (stdlog) {
    fprintf(stdlog, "LL %s\n", message);
    fflush(stdlog);
  }
}


/** TUI specific data frame handler */
static
void frame_handler(const frame_t *frame, void *data __attribute__ ((unused)))
{
  fmlog("Received Frame of type %c (%d=0x%x), size %d=0x%x",
	frame->type, frame->type, frame->type, frame->size, frame->size);
  fmlog_data((void *)frame->payload, frame->size);
}


/** Initialize TTY stuff */
void tui_init()
{
  tty_init();

  fmlog("MOO!");

  stdlog = fopen("std.log", "w");
  fprintf(stdlog, "stdlog=%p\n", (void*)stdlog);
  fmlog_set_handler(tui_log_handler, NULL);

  frame_set_handler(frame_handler, NULL);
}


/** Set up select() data structure with (ncurses based) text UI */
int tui_select_set_in(fd_set *in_fdset, int maxfd)
{
  FD_SET(STDIN_FILENO, in_fdset);
  if (STDIN_FILENO > maxfd) return STDIN_FILENO;
  else return maxfd;
}


/** Do TUI's IO stuff if necessary (from select loop) */
void tui_select_do_io(fd_set *in_fdset)
{
  /* user interface do_io */
  if (FD_ISSET(STDIN_FILENO, in_fdset)) {
    const int bytes_to_read = read_size(STDIN_FILENO);
    if (bytes_to_read == 0) {
      fmlog("EOF from stdin, exiting.\n");
      exit(0);
    }
    assert (bytes_to_read > 0);
    char buf[bytes_to_read+1];
    const ssize_t read_bytes = read(STDIN_FILENO, buf, sizeof(buf));
    assert(read_bytes == bytes_to_read);
    buf[bytes_to_read] = '\0';
    fmlog("Received %d bytes from fd %d: %s", read_bytes, STDIN_FILENO, buf);
    fmlog_data(buf, read_bytes);
    for (ssize_t i=0; i<read_bytes; i++) {
      /* handle a few key input things internally */
      switch (buf[i]) {
      case 3: /* ctrl-c */
      case 27: /* escape */
      case 'q':
      case 'Q':
      case 'x':
      case 'X':
	quit_flag = true;
	break;
      default:
	dev_write(&buf[i], read_bytes);
	break;
      }
    }
  }
}


/** TUI specific cleanup function
 *
 * Most important task is to reset the terminal state to something
 * usable, as we mess with it quite seriously.q
 */
static
void atexit_func(void)
{
  fmlog_reset_handler();
  tty_reset(tty_savefd);
  fmlog("freemcan-text: atexit_func()");
}



/** TUI's main program with select(2) based main loop */
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

  if (0 != atexit(atexit_func)) {
    fmlog_error("atexit() failed");
    abort();
  }

  /** device init */
  dev_init(device_name);

  /** initialize output module */
  tui_init();

  /** initialize signal stuff */
  sigabrt_init();

  /** main loop */
  int counter = 0;
  fmlog("Entering main loop... (%d)", counter++);

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
      fmlog("select(2) timeout\n");
      abort();
    } else { /* n>0 */
      dev_select_do_io(&in_fdset);
      tui_select_do_io(&in_fdset);
    }

    if (sigint || quit_flag) {
      break;
    }

  } /* main loop */

  /* clean up */
  dev_fini();

  return 0;
}

