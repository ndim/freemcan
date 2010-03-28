/** \file hostware/freemcan-tui.c
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
 *
 * \defgroup hostware_tui Text User Interface (TUI)
 * \ingroup hostware
 *
 * The text user interface is an interactive frontend to the MCA
 * hardware for the text console.
 *
 * @{
 */


#include <stdbool.h>

#include <unistd.h>

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

#include "compiler.h"

#include "freemcan-device.h"
#include "freemcan-frame.h"
#include "freemcan-packet.h"
#include "freemcan-export.h"
#include "freemcan-iohelpers.h"
#include "freemcan-log.h"
#include "freemcan-tui.h"


/* Forward declaration */
static void packet_handler_status(const char *status, void *data);
static void packet_handler_text(const char *text, void *data);
static void packet_handler_histogram(packet_histogram_t *histogram_packet, void *data);


/** Quit flag for the main loop. */
bool quit_flag = false;


/************************************************************************/
/** \section tui_tty TTY Setup (And Cleanup!) For The Local Interactive Terminal
 * @{
 */
/************************************************************************/


/** \brief Saved TTY fd
 *
 * Doubles as a flag indicating whether there is a termios backup in
 * #tty_save_termios or not.
 */
static int tty_savefd = -1;


/** Saved termios data */
static struct termios tty_save_termios;


/** Put terminal into raw mode.
 *
 * Stevens, page 354, Program 11.10
 */
static int tty_raw(int fd);
static int tty_raw(int fd)
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


static void tty_init();
static void tty_init()
{
  assert(tty_raw(STDIN_FILENO) >= 0);
}


/** Restore terminal mode.
 *
 * Stevens, page 355, Program 11.10
 */
static int tty_reset();
static int tty_reset()
{
  if (tty_savefd < 0) {
    return 0;
  }

  if (tcsetattr(tty_savefd, TCSAFLUSH, &tty_save_termios) < 0) {
    return -1;
  }

  return 0;
}


/** @} */


/************************************************************************/
/** \section tui_code Text User Interface
 * @{
 */
/************************************************************************/


/** Handle ABRT signal */
static void sigabrt_handler(int i __attribute__((unused)))
{
  tty_reset();
  fprintf(stderr, "SIGABRT\n");
}


/** Initialize ABRT signal handling */
static void sigabrt_init(void) __attribute__((constructor));
static void sigabrt_init(void)
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
    /* We could print a timestamp in front of the message string here */
    fprintf(stdlog, "%s\n", message);
    fflush(stdlog);
  }
}


/** Initialize TTY stuff */
void tui_init()
{
  tty_init();

  stdlog = fopen("freemcan-tui.log", "w");
  fmlog_set_handler(tui_log_handler, NULL);

  packet_set_handlers(packet_handler_histogram,
		      packet_handler_status,
		      packet_handler_text,
		      NULL);

  fmlog("Text user interface (TUI) set up");
}


void tui_fini()
{
  packet_reset_handlers();
  tty_reset();
  fmlog_reset_handler();
  if (stdlog) {
    fclose(stdlog);
    stdlog = NULL;
  }
}



/**
 * \defgroup freemcan_tui_io TUI IO handling
 * \ingroup hostware_tui
 * @{
 */

/** Do TUI's IO stuff if necessary (from select or poll loop)
 */
void tui_do_io(void)
{
    const int bytes_to_read = read_size(STDIN_FILENO);
    if (bytes_to_read == 0) {
      fmlog("EOF from stdin, exiting.\n");
      exit(EXIT_SUCCESS);
    }
    assert (bytes_to_read > 0);
    char buf[bytes_to_read+1];
    const ssize_t read_bytes = read(STDIN_FILENO, buf, sizeof(buf));
    assert(read_bytes == bytes_to_read);
    buf[bytes_to_read] = '\0';
    fmlog("Received %d bytes from fd %d", read_bytes, STDIN_FILENO);
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
      case '1':
	enable_layer1_dump = !enable_layer1_dump;
	fmlog("Layer 1 data dump now %s", enable_layer1_dump?"enabled":"disabled");
	break;
      case '2':
	enable_layer2_dump = !enable_layer2_dump;
	fmlog("Layer 2 data dump now %s", enable_layer2_dump?"enabled":"disabled");
	break;
      case '?':
      case 'h':
      case 'H':
	fmlog("Key                     Action");
        fmlog("C-c, esc, q, Q, x, X    quit program");
        fmlog("h, H, ?                 show this help message");
        fmlog("1                       toggle hexdump of received layer 1 data (byte stream)");
        fmlog("2                       toggle hexdump of received layer 2 data (frames)");
        fmlog("a                       send command \"(a)bort\"");
        fmlog("i                       send command \"(i)ntermediate result\"");
        fmlog("m                       send command \"start (m)easurement\" (short runtime)");
        fmlog("M                       send command \"start (m)easurement\" (long runtime)");
        fmlog("r                       send command \"(r)eset\"");
	break;
      case FRAME_CMD_ABORT:
      case FRAME_CMD_INTERMEDIATE:
      case FRAME_CMD_RESET:
	tui_device_send_command(buf[i], 0);
	break;
      case FRAME_CMD_MEASURE: /* 'm' */
	/* "SHORT" measurement */
	tui_device_send_command(FRAME_CMD_MEASURE, 10);
	break;
      case 'M': /* 'm' */
	/* "LONG" measurement */
	tui_device_send_command(FRAME_CMD_MEASURE, 30);
	break;
      /* No "default:" case as we ignore all other characters. */
      }
    }
}

/** @} */


/** TUI specific cleanup function
 *
 * Most important task is to reset the terminal state to something
 * usable, as we mess with it quite seriously.q
 */
static
void atexit_func(void)
{
  tui_fini();
}

/** @} */


/************************************************************************/
/** \defgroup tui_data_handling TUI Data Handling
 * \ingroup freemcan_tui
 *
 * Application Layer (Communication Layer 4).
 *
 * @{
 */
/************************************************************************/


/** Status data packet handler (TUI specific) */
static void packet_handler_status(const char *status, void *UP(data))
{
  fmlog("STATUS: %s", status);
}


/** Text data packet handler (TUI specific) */
static void packet_handler_text(const char *text, void *UP(data))
{
  fmlog("TEXT: %s", text);
}


/** Histogram data packet handler (TUI specific) */
static void packet_handler_histogram(packet_histogram_t *histogram_packet,
				     void *UP(data))
{
  packet_histogram_ref(histogram_packet);

  const size_t element_count = histogram_packet->element_count;
  const packet_histogram_type_t type = histogram_packet->type;
  char buf[128];
  if ((type>=32)&&(type<127)) {
    snprintf(buf, sizeof(buf),
	     "Received '%c' type histogram: %%d elements, %%d seconds:",
	     type);
  } else {
    snprintf(buf, sizeof(buf),
	     "Received 0x%02x=%d type histogram: %%d elements, %%d seconds:",
	     type, type);
  }
  fmlog(buf, element_count, histogram_packet->duration);
  fmlog_hist(histogram_packet->elements, element_count);

  /* export current histogram to file(s) */
  export_histogram(histogram_packet);

  packet_histogram_unref(histogram_packet);
}

/** @} */


/** Init TUI main loop (select/poll independent)
 *
 * Parse command line parameters, etc.
 */
const char *main_init(int argc, char *argv[])
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

  if (0 != atexit(atexit_func)) {
    fmlog_error("atexit() failed");
    abort();
  }

  return argv[1];
}


/** @} */
