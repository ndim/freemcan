/** \file freemcan-ncurses.c
 * \brief Freemcan text user interface (ncurses based)
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


#define _BSD_SOURCE

#if 0
#include "config.h"
#include "include/system.h"
#include "include/i18n.h"
#endif

#include <unistd.h>
#include <time.h>
#include <curses.h>

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

#include "freemcan.h"
#include "freemcan-common.h"
#include "freemcan-device.h"
#include "freemcan-frame.h"
#include "freemcan-log.h"
#include "freemcan-select.h"
#include "freemcan-tui.h"



static void sigabrt_handler(int i __attribute__((unused)))
{
  endwin();
  fprintf(stderr, "SIGABRT\n");
}

static void sigabrt_init()
{
  sighandler_t abrt_handler __attribute__((unused))
    = signal(SIGABRT, sigabrt_handler);
  /* linux will try and restart an interrupted system call by default */
  siginterrupt(SIGABRT, 1); /* stop system calls on SIGABRT */
}


/** flag to be set on SIGWINCH */
static
bool sigwinch = false;

/** our SIGWINCH handler (window size change) */
static
void sigwinch_handler(int i __attribute__((unused)))
{
  sigwinch = true;
}

/** initialize SIGWINCH stuff
 * \bug Do we really want to continue syscalls?
 */
static
void sigwinch_init()
{
  sighandler_t winch_handler __attribute__((unused))
    = signal(SIGWINCH, sigwinch_handler);
  /* linux will try and restart an interrupted system call by default */
  siginterrupt(SIGWINCH, 1); /* stop system calls on SIGWINCH */
}



/** screen element with ncurses window and DPS */
typedef struct {
  WINDOW *window;
  freemcan_t *dps;
} screen_element_t;


/** curses terminal window */
static WINDOW *mainwnd;

/** curses logging window */
static WINDOW *logwnd;


/** terminal window size, updated via SIGWINCH handler */
static struct winsize window_size;


/** initialize curses stuff */
static
void tui_update_window_size()
{
  if (ioctl(STDOUT_FILENO, TIOCGWINSZ, (char *) &window_size) < 0) {
    fmlog_error("TIOCGWINSZ error on stdout");
    abort();
  }
  fmlog("Window size: row:%d col:%d xpixel:%d ypixel:%d",
	window_size.ws_row, window_size.ws_col,
	window_size.ws_xpixel, window_size.ws_ypixel);
}


FILE *stdlog = NULL;


static
ssize_t tui_log_handler(void *data, 
			const char *message, 
			const size_t length __attribute__ (( unused )))
{
  WINDOW *wnd = (WINDOW *)(data);
  int i = waddstr(wnd, message);
  int j = waddstr(wnd, "\n");
  wrefresh(wnd);
  if (stdlog) {
    fprintf(stdlog, "[[@%p|%d,%d]] %s\n", (void *)wnd, i, j, message);
    fflush(stdlog);
  }
  return ((i<0)||(j<0))?-1:i+j;
}


/** array of screen elements */
static
screen_element_t *sel;


/** initialize curses stuff */
void tui_init()
{
  tui_update_window_size();

  mainwnd = initscr();
  if (!has_colors()) {
    endwin();
    fmlog("Your terminal does not support color");
    abort();
  }
  start_color();
  init_pair(1, COLOR_CYAN, COLOR_BLACK);

  nocbreak();printw("A Big string which i didn't care to type fully ");
  mvchgat(0, 0, -1, A_BOLD, 1, NULL);

  raw();
  keypad(stdscr, TRUE);
  noecho();

  nodelay(mainwnd, false);
  attron(COLOR_PAIR(1)|A_BOLD);
  int rows, cols;
  getmaxyx(stdscr, rows, cols);
  const char *msg = "Moooooooo!";
  int s = mvwprintw(mainwnd, rows/2, (cols - strlen(msg))/2, "%s", msg);
  wprintw(mainwnd, "\nMoooooooo! %d\nPress any key to continue\n", s);
  attroff(COLOR_PAIR(1)|A_BOLD);
  waddch(mainwnd, 'x');
  waddch(mainwnd, 'x' | A_BOLD);
  waddch(mainwnd, 'x' | A_UNDERLINE);
  wrefresh(mainwnd);
  getch();
  endwin();
  exit(0);

  wrefresh(mainwnd);

  if (1) {
    const int nlines  = 6, ncols   = window_size.ws_col;
    const int begin_x = 0, begin_y = window_size.ws_row - nlines;
    WINDOW *logwnd_frame = newwin(nlines, ncols, begin_y, begin_x);
    box(logwnd, ACS_VLINE, ACS_HLINE);
    mvwprintw(logwnd_frame, 0, 2, "[ Log ]");
    wrefresh(logwnd_frame);
    logwnd = derwin(logwnd_frame, 1, 1, nlines-2, ncols-2);
    int r = wprintw(logwnd, "Testing...");
    stdlog = fopen("std.log", "w");
    fprintf(stdlog, "wprintw returned %d, stdlog=%p\n", r, (void*)stdlog);
    fmlog_set_handler(tui_log_handler, (void *)logwnd);
    wrefresh(logwnd);
  }
  curs_set(0);
}


/** handle arriving packet from DPS */
static
void tui_packet_handler(const frame_t *const p, void *data)
{
  screen_element_t *sel = (screen_element_t *)(data);
  WINDOW *window = sel->window;
  mvwprintw(window,  1, 2, "current   limits");
  mvwprintw(window,  2, 2, "frame_t * %p", p);
  /*
  mvwprintw(window,  2, 2, "%05.2f V  %05.2f V    %s", 
	    p->u, p->lim_u, (p->remote?"RS232 access":"LOCAL access"));
  mvwprintw(window,  3, 2, "%05.3f A  %05.3f A    %s", 
	    p->i, p->lim_i, (p->output?"output ON ":"output N/C"));
  mvwprintw(window,  4, 2, "%05.1f W  %05.1f W    %s", 
	    p->p, p->lim_p, (p->overtemp?"OVERTEMP":"temp ok"));
  */
  wrefresh(window);
  refresh();
}


/** Set up select() data structure with (ncurses based) text UI */
int tui_select_set_in(fd_set *in_fdset, int maxfd)
{
  FD_SET(STDIN_FILENO, in_fdset);
  if (STDIN_FILENO > maxfd) return STDIN_FILENO;
  else return maxfd;
}


/** Do (ncurses based) text UI's IO stuff if necessary (from select loop) */
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
    fmlog("Received %d bytes from fd %d: %s\n", read_bytes, STDIN_FILENO, buf);
    fmlog_data(buf, read_bytes);
    dev_write(buf, read_bytes);
  }
}


/** rearrange user interface windows after screen size change
 * \bug To be implemented.
 */
static
void tui_rearrange_windows(void)
{
}





static
void atexit_func(void)
{
  fmlog_reset_handler();
  endwin();
  fmlog("dps-dumper-ncurses: atexit_func()");
}



/** main program with select(2) based main loop */
int main(int argc, char *argv[])
{
  const int first_arg = 1;
  assert(argc > first_arg);
  assert(isatty(STDIN_FILENO));
  assert(isatty(STDOUT_FILENO));

  if (0 != atexit(atexit_func)) {
    fmlog_error("atexit() failed");
    abort();
  }

  /** device init */
  dev_init(device_name);

  /** initialize output module */
  tui_init();
  sel[argc-first_arg].window = logwnd;

  /** initialize signal stuff */
  sigwinch_init();
  sigabrt_init();

  /** create the ncurses windows belonging to the dps_t objects */
  for (int i=0; i+first_arg<argc; i++) {
    int nlines  = 6,          ncols   = 36;
    int begin_x = 2+40*(i%2), begin_y =  0;
    const char *const dev_fname = argv[first_arg+i];
    sel[i].window = newwin(nlines, ncols, begin_y, begin_x);
    box(sel[i].window, ACS_VLINE, ACS_HLINE);
    mvwprintw(mainwnd, begin_y, begin_x+2, "[ DPS @ %s ]", dev_fname);
  }


  /** main loop */
  int counter = 0;
  fmlog("Entering main loop... (%d)", counter++);

  while (1) {
    fd_set in_fdset;
    FD_ZERO(&in_fdset);
    FD_SET(STDIN_FILENO, &in_fdset);
    int max_fd = STDIN_FILENO;
    max_fd = dps_select_set(&in_fdset, max_fd);
    struct timeval timeout = { .tv_sec = 5, .tv_usec = 0 };

    int n = select(max_fd+1, &in_fdset, NULL, NULL, &timeout);
    if (n<0) {
      if (errno != EINTR) {
	fmlog_error("error running select(2)");
	abort();
      }
    } else if (0 == n) {
      /* timeout */
      fmlog("Moo timeout! %d", counter++);
    } else { /* n > 0 */
      /* data ready to be read */
      dps_select_do_io(&in_fdset);
      tui_select_do_io(&in_fdset);
    }

    if (sigint) {
      break;
    }
    if (sigwinch) {
      tui_update_window_size();
      tui_rearrange_windows();
    }
  } /* main loop */

  /* clean up */
  for (int i=0; i<argc-first_arg; i++) {
    dps_unref(sel[i].dps);
  }
  free(sel);

  return 0;
}

