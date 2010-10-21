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

#include <math.h>

#include <unistd.h>

#include <termios.h>
#include <sys/ioctl.h>

#include "compiler.h"

#include "frame-defs.h"
#include "frame-parser.h"
#include "packet-parser.h"

#include "freemcan-device.h"
#include "freemcan-packet.h"
#include "freemcan-export.h"
#include "freemcan-iohelpers.h"
#include "freemcan-log.h"
#include "freemcan-tui.h"

#include "git-version.h"


/* Forward declaration */
static void packet_handler_state(const char *state, void *data);
static void packet_handler_text(const char *text, void *data);
static void packet_handler_value_table(packet_value_table_t *value_table_packet, void *data);


bool is_measuring = false;
int waiting_for = 0;


/** Quit flag for the main loop. */
bool quit_flag = false;


/** Whether to dump the user input into log */
bool enable_user_input_dump = false;


/** Whether to trigger periodic updates */
bool periodic_update_flag = false;


/** Interval in seconds */
unsigned long periodic_update_interval = 20;


/** Size of last received packet */
size_t last_received_size = 0;


/** Last sent duration, used for calculating periodic update interval */
uint16_t last_sent_duration = 0;


static void recalculate_periodic_interval(void)
{
  const unsigned long last_periodic_update_interval = periodic_update_interval;
  if (last_sent_duration) {
    float tmp = 1.5*sqrt(last_sent_duration);
    periodic_update_interval = tmp + ((last_received_size) / 11520UL);
    if (periodic_update_interval < 5) {
      periodic_update_interval = 5;
    }
  } else {
    periodic_update_interval = 20;
  }
  if (last_periodic_update_interval != periodic_update_interval) {
    fmlog("Periodic update interval updated from %lu to %lu",
          last_periodic_update_interval, periodic_update_interval);
  }
}


void update_last_received_size(const uint16_t size)
{
  if (size >= last_received_size) {
    last_received_size = size;
  }
  recalculate_periodic_interval();
}



/** \section tui_durations TUI Measurement Duration handling
 * @{
 */


/** Entry for a list of measurement durations */
typedef struct {
  uint16_t short_duration;
  uint16_t long_duration;
} duration_T;


/** List of measurement durations */
const duration_T duration_list[] = {
  { 2, 3},
  { 10, 30},
  { 60, 150},
  { 600, 1200},
  { 3600, 3*3600},
  { 0, 0 }
};


/** Index into list of measurement durations */
unsigned int duration_index = 0;


/** Log current measurement durations */
static
void fmlog_durations(void)
{
  fmlog("Measurement durations in seconds: short=%u, long=%u",
        duration_list[duration_index].short_duration,
        duration_list[duration_index].long_duration);
}


/** @} */


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


static
void tui_fmlog_help(void)
{
  fmlog("Key         Action");
  fmlog("q, Q, x, X  quit program (other keys: C-c, esc)");
  fmlog("h, H, ?     show this help message");
  fmlog("1           toggle hexdump of received layer 1 data (byte stream)");
  fmlog("2           toggle hexdump of received layer 2 data (frames)");
  fmlog("9           toggle dump of user input (typed characters)");
  fmlog("+/-         increase/decrease measurement duration of 'm/M' command");
  fmlog("a           send command \"(a)bort\"");
  fmlog("i           send command \"(i)ntermediate result\"");
  fmlog("m           send command \"start (m)easurement\" (short duration: %u seconds)",
        duration_list[duration_index].short_duration);
  fmlog("M           send command \"start (m)easurement\" (long duration: %u seconds)",
        duration_list[duration_index].long_duration);
  fmlog("p           toggle (p)eriodical requests of intermediate results");
  fmlog("r           send command \"(r)eset\"");
  fmlog("w           send command \"intermediate result\" and (w)rite data to file");
}


void tui_startup_messages(void)
{
  tui_fmlog_help();
}


packet_parser_t *tui_packet_parser = NULL;


/** Initialize TTY stuff */
void tui_init()
{
  tty_init();

  stdlog = fopen("freemcan-tui.log", "w");
  fmlog_set_handler(tui_log_handler, NULL);

  tui_packet_parser = packet_parser_new(packet_handler_value_table,
                                        packet_handler_state,
                                        packet_handler_text,
                                        NULL);

  fmlog("freemcan TUI " GIT_VERSION);
  fmlog("Text user interface (TUI) set up");
}


void tui_fini()
{
  /** \bug: Hack: Avoid running tui_fini() twice using a flag. */
  static volatile bool tui_fini_run = false;
  if (!tui_fini_run) {
    packet_parser_unref(tui_packet_parser);
    tty_reset();
    fmlog_reset_handler();
    if (stdlog) {
      fclose(stdlog);
      stdlog = NULL;
    }
    tui_fini_run = true;
  }
}



/**
 * \defgroup freemcan_tui_io TUI IO handling
 * \ingroup hostware_tui
 * @{
 */


void tui_do_timeout(void)
{
  if (waiting_for > 2) {
    /* Not connected, apparently. Implies not measuring, either. */
    is_measuring = false;
  }
  if (is_measuring) {
    tui_device_send_simple_command(FRAME_CMD_INTERMEDIATE);
  }
}


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
    if (enable_user_input_dump) {
      fmlog("Received %d bytes from fd %d", read_bytes, STDIN_FILENO);
      fmlog_data(buf, read_bytes);
    }
    for (ssize_t i=0; i<read_bytes; i++) {
      /* handle a few key input things internally */
      switch (buf[i]) {
      case 3: /* ctrl-c */
      /* case 27: escape
       *
       * Pressing e.g. cursor up sends an escape character and thus
       * would quit freemcan-tui. So we ignore escape characters.
       */
      case 'q':
      case 'Q':
      case 'x':
      case 'X':
        fmlog("Quitting the program.");
        quit_flag = true;
        break;
      case 'p':
        periodic_update_flag = !periodic_update_flag;
        if (periodic_update_flag) {
          recalculate_periodic_interval();
          fmlog("Periodic updates now enabled (every %lu seconds)",
                periodic_update_interval);
          tui_device_send_simple_command(FRAME_CMD_INTERMEDIATE);
        } else {
          fmlog("Periodic updates now disabled");
        }
        break;
      case '1':
        enable_layer1_dump = !enable_layer1_dump;
        fmlog("Layer 1 data dump now %s", enable_layer1_dump?"enabled":"disabled");
        break;
      case '2':
        enable_layer2_dump = !enable_layer2_dump;
        fmlog("Layer 2 data dump now %s", enable_layer2_dump?"enabled":"disabled");
        break;
      case '9':
        enable_user_input_dump = !enable_user_input_dump;
        fmlog("User input dump now %s", enable_user_input_dump?"enabled":"disabled");
        break;
      case '?':
      case 'h':
      case 'H':
        tui_fmlog_help();
        break;
      case '+':
        if (duration_list[duration_index+1].short_duration != 0) {
          ++duration_index;
          fmlog_durations();
        }
        break;
      case '-':
        if (duration_index > 0) {
          --duration_index;
          fmlog_durations();
        }
        break;
      case FRAME_CMD_MEASURE: /* 'm' */
        /* "SHORT" measurement */
        last_sent_duration = duration_list[duration_index].short_duration;
        recalculate_periodic_interval();
        tui_device_send_measure_command(last_sent_duration);
        break;
      case 'M': /* 'm' */
        /* "LONG" measurement */
        last_sent_duration = duration_list[duration_index].long_duration;
        recalculate_periodic_interval();
        tui_device_send_measure_command(last_sent_duration);
        break;
      case FRAME_CMD_ABORT:
      case FRAME_CMD_RESET:
      case FRAME_CMD_STATE:
      case ' ':
        tui_device_send_simple_command(buf[i]);
        break;
      case 'i':
        tui_device_send_simple_command(FRAME_CMD_INTERMEDIATE);
        break;
      case 'w':
        write_next_intermediate_packet = true;
        tui_device_send_simple_command(FRAME_CMD_INTERMEDIATE);
        break;
      default:
        /* Ignore all other input characters, but print a warning. */
        if (1) {
          const uint8_t u = buf[i];
          if ((u >= 32) && (u < 127)) {
            fmlog("Ignoring input byte 0x%02x=%u='%c'", u, u, (char)u);
          } else {
            fmlog("Ignoring input byte 0x%02x=%u", u, u);
          }
        }
        break;
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


/** State data packet handler (TUI specific) */
static void packet_handler_state(const char *state, void *UP(data))
{
  if (waiting_for > 0) {
    waiting_for--;
  }
  fmlog("STATE: %s", state);
  bool new_is_measuring = (strcmp("MEASURING", state) == 0);
  if (new_is_measuring != is_measuring) {
    if (new_is_measuring) {
    } else {
    }
    is_measuring = new_is_measuring;
  }
}


/** Text data packet handler (TUI specific) */
static void packet_handler_text(const char *text, void *UP(data))
{
  if (waiting_for > 0) {
    waiting_for--;
  }
  fmlog("TEXT: %s", text);
}


/** Value table data packet handler (TUI specific) */
static void packet_handler_value_table(packet_value_table_t *value_table_packet,
                                       void *UP(data))
{
  if (waiting_for > 0) {
    waiting_for--;
  }
  packet_value_table_ref(value_table_packet);

  const size_t element_count = value_table_packet->element_count;
  const packet_value_table_reason_t reason = value_table_packet->reason;
  const packet_value_table_type_t type = value_table_packet->type;
  last_sent_duration = value_table_packet->total_duration;
  char buf[128];
  char reason_str[16];
  if ((reason>=32)&&(reason<127)) {
    snprintf(reason_str, sizeof(reason_str), "'%c'", reason);
  } else {
    snprintf(reason_str, sizeof(reason_str), "0x%02x=%d", reason, reason);
  }
  char type_str[16];
  if ((type>=32)&&(type<127)) {
    snprintf(type_str, sizeof(type_str), "'%c'", type);
  } else {
    snprintf(type_str, sizeof(type_str), "0x%02x=%d", type, type);
  }
  snprintf(buf, sizeof(buf),
           "Received %s type value table for reason %s: %%d elements, %%d seconds:",
           type_str, reason_str);

  fmlog(buf, element_count, value_table_packet->duration);
  fmlog_value_table(value_table_packet->elements, element_count);

  /* export current value table to file(s) */
  export_value_table(value_table_packet);

  packet_value_table_unref(value_table_packet);
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


/*
 * Local Variables:
 * c-basic-offset: 2
 * indent-tabs-mode: nil
 * End:
 */
